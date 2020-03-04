use clap::{App, Arg, AppSettings};
use std::ffi::OsStr;
use std::borrow::Cow;
use esl::*;
use esl::code::*;
use std::io::{Write, stdout, stdin, BufRead, BufReader, BufWriter};
use std::mem::transmute;
use esl::read::*;
use std::fs::File;

#[cfg(target_os = "windows")]
const DEFAULT_NEWLINE: &'static str = "dos";
#[cfg(not(target_os = "windows"))]
const DEFAULT_NEWLINE: &'static str = "unix";

fn main() {
    let args = App::new("ESP Assembler/Disassembler")
        .version(env!("CARGO_PKG_VERSION"))
        .template("Usage: {usage}\n{about}\n\n{unified}\n\n{after-help}")
        .after_help("<COND> can be in one of the following form: <RECORD_TAG>, <RECORD_TAG>:<FIELD_TAG>, or :<FIELD_TAG>\n\n\
            Report bugs to <internalmike@gmail.com> (in English or Russian).\n\
            ESP Assembler/Disassembler home page: <https://github.com/A1-Triard/esp-assembler>.\
        ")
        .about("Convert FILEs from the .esm/.esp/.ess format to YAML and back.")
        .help_message("display this help and exit")
        .version_message("display the version number and exit")
        .arg(Arg::with_name("FILE")
            .multiple(true)
            .help("input file, '-' stands for standard input")
        )
        .arg(Arg::with_name("disassemble")
            .short("d")
            .long("disassemble")
            .help("convert binary .es{s,p,m} file to YAML")
        )
        .arg(Arg::with_name("verbose")
            .short("v")
            .long("verbose")
            .help("verbose mode")
        )
        .arg(Arg::with_name("fit")
            .short("f")
            .long("fit")
            .help("remove redundant trailing zeros and other garbage")
        )
        .arg(Arg::with_name("exclude")
            .short("e")
            .long("exclude")
            .multiple(true)
            .value_name("COND")
            .help("skip specified records/fields")
        )
        .arg(Arg::with_name("include")
            .short("i")
            .long("include")
            .multiple(true)
            .value_name("COND")
            .help("skip all but specified records/fields")
        )
        .arg(Arg::with_name("code_page")
            .short("c")
            .long("code-page")
            .value_name("LANG")
            .possible_value("en")
            .possible_value("ru")
            .required(true)
            .help("text code page")
        )
        .arg(Arg::with_name("keep")
            .short("k")
            .long("keep")
            .help("keep (don't delete) input files")
        )
        .arg(Arg::with_name("newline")
            .short("n")
            .long("newline")
            .value_name("NL")
            .possible_value("unix")
            .possible_value("dos")
            .requires("disassemble")
            .help("newline style [default: dos]")
        )
        .setting(AppSettings::DontCollapseArgsInUsage)
        .get_matches()
    ;
    let files = args.values_of_os("FILE").map_or_else(Vec::new, |v| v.collect());
    let exclude = args.values_of("exclude").map_or_else(Vec::new, |v| v.collect());
    let include = args.values_of("include").map_or_else(Vec::new, |v| v.collect());
    let fit = args.is_present("fit");
    let disassemble = if args.is_present("disassemble") {
        Some(match args.value_of("newline").unwrap_or(DEFAULT_NEWLINE) {
            "dos" => "\r\n",
            "unix" => "\n",
            _ => unreachable!()
        })
    } else {
        None
    };
    let verbose = args.is_present("verbose");
    let code_page = match args.value_of("code_page").unwrap() {
        "en" => CodePage::English,
        "ru" => CodePage::Russian,
        _ => unreachable!()
    };
    for &file in files.iter() {
        if let Err(e) = espa(file, &exclude, &include, fit, disassemble, verbose, code_page) {
            eprintln!("{}.", e);
        }
    }
}

static ESL_SUFFIXES: &'static [&'static [u8]] = &[b".ess", b".ESS", b".esp", b".ESP", b".esm", b".ESM"];

fn get_output_name(input_name: &OsStr, disassemble: bool) -> Result<Cow<OsStr>, String> {
    if input_name == "-" { return Ok(input_name.into()) }
    let input_name: &[u8] = unsafe { transmute(input_name) };
    let is_yaml = input_name.ends_with(b".yaml");
    if disassemble {
        if is_yaml {
            let input_name: &OsStr = unsafe { transmute(input_name) };
            Err(format!("{} already has .yaml suffix, skipping", input_name.to_string_lossy()))
        } else if ESL_SUFFIXES.iter().any(|s| input_name.ends_with(s)) {
            let mut output_name = Vec::with_capacity(input_name.len() + b".yaml".len());
            output_name.extend_from_slice(input_name);
            output_name.extend_from_slice(b".yaml");
            let output_name = &output_name[..];
            let output_name: &OsStr = unsafe { transmute(output_name) };
            Ok(output_name.to_os_string().into())
        } else {
            let input_name: &OsStr = unsafe { transmute(input_name) };
            Err(format!("{}: unknown suffix, skipping", input_name.to_string_lossy()))
        }
    } else {
        if !is_yaml {
            let input_name: &OsStr = unsafe { transmute(input_name) };
            Err(format!("{}: unknown suffix, skipping", input_name.to_string_lossy()))
        } else {
            let output_name = &input_name[.. input_name.len() - b".yaml".len()];
            if ESL_SUFFIXES.iter().any(|s| output_name.ends_with(s)) {
                let output_name: &OsStr = unsafe { transmute(output_name) };
                Ok(output_name.into())
            } else {
                let input_name: &OsStr = unsafe { transmute(input_name) };
                Err(format!("{}: unknown suffix, skipping", input_name.to_string_lossy()))
            }
        }
    }
}

fn assembly_record(lines: &str, output: &mut dyn Write, code_page: CodePage) -> Result<(), String> {
    let records: Vec<Record> = serde_yaml::from_str(&lines).map_err(|e| format!("{}", e))?;
    for record in records.iter() {
        serialize_into(record, output, code_page, false).map_err(|e| format!("{}", e))?;
    }
    Ok(())
}

fn espa(
    input_name: &OsStr,
    _exclude: &[&str],
    _include: &[&str],
    _fit: bool, 
    disassemble: Option<&str>,
    _verbose: bool,
    code_page: CodePage
) -> Result<(), String> { 
    let output_name = get_output_name(input_name, disassemble.is_some())?;
    let stdin = stdin();
    let mut input: Box<dyn BufRead> = if input_name == "-" {
        Box::new(stdin.lock())
    } else {
        Box::new(BufReader::new(File::open(input_name).map_err(|e| format!("{}: {}", input_name.to_string_lossy(), e))?))
    };
    let stdout = stdout();
    let mut output: Box<dyn Write> = if input_name == "-" {
        Box::new(stdout.lock())
    } else {
        Box::new(BufWriter::new(File::create(&output_name).map_err(|e| format!("{}: {}", output_name.to_string_lossy(), e))?))
    };
    if let Some(newline) = disassemble {
        for record in Records::new(code_page, 0, &mut input) {
            match record {
                Err(e) => return Err(format!("{}: {}", input_name.to_string_lossy(), e)),
                Ok(record) => {
                    let record = serde_yaml::to_string(&record).unwrap();
                    let record = record[4..].replace("\n", &(newline.to_string() + "  "));
                    write!(output, "- {}{}", record, newline).map_err(|e| format!("{}", e))?;
                }
            }
        }
    } else {
        let mut lines = String::with_capacity(128);
        for line in input.lines() {
            match line {
                Err(e) => return Err(format!("{}: {}", input_name.to_string_lossy(), e)),
                Ok(line) => {
                    if line.chars().nth(0) == Some('-') && !lines.is_empty() {
                        assembly_record(&lines, &mut output, code_page)?;
                        lines.clear();
                    }
                    lines += &line;
                    lines.push('\n');
                }
            }
        }
        if !lines.is_empty() {
            assembly_record(&lines, &mut output, code_page)?;
        }
    }
    Ok(())
}
