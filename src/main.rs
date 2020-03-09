#![feature(const_transmute)]
use clap::{App, Arg, AppSettings, ArgMatches};
use std::ffi::{OsStr, OsString};
use esl::*;
use esl::code::*;
use std::io::{Write, stdout, stdin, BufRead, BufReader, BufWriter};
use std::mem::transmute;
use esl::read::*;
use std::fs::{File, remove_file, rename};
use uuid::Uuid;
use std::path::{Path, PathBuf};
use either::{Either, Left, Right};
use std::process::exit;
use std::str::FromStr;
use std::fmt::Display;

#[cfg(target_os = "windows")]
const DEFAULT_NEWLINE: &'static str = "dos";
#[cfg(not(target_os = "windows"))]
const DEFAULT_NEWLINE: &'static str = "unix";

struct Options {
    exclude_records: Vec<Tag>,
    exclude_fields: Vec<(Option<Tag>, Tag)>,
    include_records: Vec<Tag>,
    include_fields: Vec<(Option<Tag>, Tag)>,
    fit: bool,
    keep: Option<bool>,
    disassemble: Option<&'static str>,
    verbose: bool,
    code_page: CodePage,
}

fn parse_cond(value: &str) -> Result<Either<Tag, (Option<Tag>, Tag)>, String> {
    let mut tags = value.split(':');
    let record_tag = tags.next().ok_or_else(|| format!("invalid COND {:?}", value))?;
    let field_tag = tags.next();
    if tags.next().is_some() { return Err(format!("invalid COND {:?}", value)); }
    let record_tag = if record_tag.is_empty() {
        None
    } else {
        Some(Tag::from_str(record_tag).map_err(|()| format!("invalid tag {:?}", record_tag))?)
    };
    if let Some(field_tag) = field_tag {
        let field_tag = Tag::from_str(field_tag).map_err(|()| format!("invalid tag {:?}", field_tag))?;
        Ok(Right((record_tag, field_tag)))
    } else {
        let record_tag = record_tag.ok_or_else(|| format!("invalid COND {:?}", value))?;
        Ok(Left(record_tag))
    }
}

fn parse_conds(args: &ArgMatches, name: &'static str) -> (Vec<Tag>, Vec<(Option<Tag>, Tag)>) {
    let mut records = Vec::new();
    let mut fields = Vec::new();
    if let Some(values) = args.values_of(name) {
        for value in values {
            match parse_cond(value) {
                Ok(Left(record_tag)) => records.push(record_tag),
                Ok(Right(field_cond)) => fields.push(field_cond),
                Err(e) => {
                    eprintln!("error: {}\n\nFor more information try --help", e);
                    exit(1);
                }
            }
        }
    }
    (records, fields)
}

const HYPHEN: &'static OsStr = unsafe { transmute("-") };

fn parse_args() -> (Options, Vec<Option<PathBuf>>) {
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
            .short("p")
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
        .arg(Arg::with_name("use_stdout")
            .short("c")
            .long("stdout")
            .conflicts_with("keep")
            .help("write on standard output, keep original files unchanged")
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
    let files = args.values_of_os("FILE").map_or_else(Vec::new, |v| v.map(|v| if v == HYPHEN {
        None
    } else {
        Some(PathBuf::from(v))
    }).collect());
    let fit = args.is_present("fit");
    let keep = if args.is_present("use_stdout") {
        None
    } else {
        Some(args.is_present("keep"))
    };
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
    let (exclude_records, exclude_fields) = parse_conds(&args, "exclude");
    let (include_records, include_fields) = parse_conds(&args, "include");
    (Options {
        fit, keep, disassemble, verbose, code_page,
        exclude_records, exclude_fields, include_records, include_fields
    },
        files
    )
}

fn main() {
    let (options, files) = parse_args();
    for file in files.iter() {
        if let Err(e) = convert_file(file.as_ref().map(|x| x.as_path()), &options) {
            eprintln!("{}.", e);
        }
    }
}

const YAML_SUFFIX: &'static OsStr = unsafe { transmute(".yaml") };
static ESL_SUFFIXES: &'static [&'static OsStr] = &[
    unsafe { transmute(".ess") }, 
    unsafe { transmute(".ESS") },
    unsafe { transmute(".esp") },
    unsafe { transmute(".ESP") },
    unsafe { transmute(".esm") },
    unsafe { transmute(".ESM") },
];

fn has_known_file_suffix(file: &Path) -> bool {
    if let Some(extension) = file.extension() {
        ESL_SUFFIXES.contains(&extension)
    } else {
        false
    }
}

fn get_disassembled_name(input_name: &Path) -> Result<PathBuf, String> {
    if input_name.extension() == Some(YAML_SUFFIX) {
        return Err(format!("{} already has .yaml suffix, skipping", input_name.display()));
    }
    if !has_known_file_suffix(input_name) {
        return Err(format!("{} does not have known suffix, skipping", input_name.display()));
    }
    let input_file_name = input_name.file_name().unwrap();
    let mut output_file_name: Vec<u8> = Vec::with_capacity(input_file_name.len() + YAML_SUFFIX.len());
    output_file_name.extend_from_slice(unsafe { transmute(input_file_name) });
    output_file_name.extend_from_slice(unsafe { transmute(YAML_SUFFIX) });
    let output_file_name: &OsStr = unsafe { transmute(&output_file_name[..]) };
    Ok(input_name.with_file_name(output_file_name))
}

fn get_assembled_name(input_name: &Path) -> Result<PathBuf, String> {
    if input_name.extension() != Some(YAML_SUFFIX) {
        return Err(format!("{} does not have .yaml suffix, skipping", input_name.display()));
    }
    let output_file_name = input_name.file_stem().unwrap();
    let output_name = input_name.with_file_name(output_file_name);
    if !has_known_file_suffix(&output_name) {
        return Err(format!("{} does not have known suffix, skipping", input_name.display()));
    }
    Ok(output_name)
}

fn get_output_name(input_name: &Path, disassemble: bool) -> Result<PathBuf, String> {
    if disassemble {
        get_disassembled_name(input_name).map(|x| x.into())
    } else {
        get_assembled_name(input_name).map(|x| x.into())
    }
}

fn with_path(out: bool, path: Option<&Path>, error: impl Display) -> String {
    if let Some(path) = path {
        format!("{}: {}", path.display(), error)
    } else {
        let name = if out { "<stdout>" } else { "<stdin>" };
        format!("{}: {}", name, error)
    }
}

fn convert_file(input_name: Option<&Path>, options: &Options) -> Result<(), String> {
    let output_name = if let Some(input_name) = input_name {
        let output_name = get_output_name(input_name, options.disassemble.is_some())?;
        if options.keep.is_none() { None } else { Some(output_name) }
    } else {
        None
    };
    let mut temp_name = None;
    let res = if let Err(e) = convert_records(input_name, output_name.as_ref().map(|x| x.as_path()), options, &mut temp_name) {
        Err(e)
    } else {
        if let Some(temp_name) = &temp_name {
            let output_name = output_name.unwrap();
            let output_name = output_name.as_path();
            rename(temp_name, output_name).map_err(|e| format!("{}: {}", output_name.display(), e))
        } else {
            Ok(())
        }
    };
    if let Err(e) = res {
        if let Some(temp_name) = temp_name.as_ref() {
            if let Err(f) = remove_file(temp_name) {
                eprintln!("{}: {}", temp_name.display(), f);
            }
        }
        Err(e)
    } else if options.keep == Some(false) {
        let input_name = input_name.unwrap();
        remove_file(input_name).map_err(|e| format!("{}: {}", input_name.display(), e))
    } else {
        Ok(())
    }
}

fn convert_records(input_name: Option<&Path>, output_name: Option<&Path>, options: &Options, temp_name: &mut Option<PathBuf>)
    -> Result<(), String> {
    
    let stdin = stdin();
    let stdout = stdout();
    let (mut input, mut output): (Box<dyn BufRead>, Box<dyn Write>) =
        if let Some(input_name) = input_name {
            let input = Box::new(BufReader::new(File::open(input_name).map_err(|e| format!("{}: {}", input_name.display(), e))?));
            if let Some(output_name) = output_name {
                let temp = input_name.with_file_name(&OsString::from(format!("{}", Uuid::new_v4().to_simple())));
                let output = Box::new(BufWriter::new(File::create(&temp).map_err(|e| format!("{}: {}", output_name.display(), e))?));
                temp_name.replace(temp);
                (input, output)
            } else {
                (input, Box::new(stdout.lock()))
            }
        } else {
            (Box::new(stdin.lock()), Box::new(stdout.lock()))
        }
    ;
    if let Some(newline) = options.disassemble {
        for record in Records::new(options.code_page, 0, &mut input) {
            match record {
                Err(e) => return Err(with_path(false, input_name, e)),
                Ok(record) => {
                    let record = serde_yaml::to_string(&record).unwrap();
                    let record = record[4..].replace("\n", &(newline.to_string() + "  "));
                    write!(output, "- {}{}", record, newline).map_err(|e| format!("{}", e))?;
                }
            }
        }
    } else {
        fn assembly_record(lines: &str, output: &mut dyn Write, code_page: CodePage) -> Result<(), String> {
            let records: Vec<Record> = serde_yaml::from_str(&lines).map_err(|e| format!("{}", e))?;
            for record in records.iter() {
                serialize_into(record, output, code_page, false).map_err(|e| format!("{}", e))?;
            }
            Ok(())
        }
        let mut lines = String::with_capacity(128);
        for line in input.lines() {
            match line {
                Err(e) => return Err(with_path(false, input_name, e)),
                Ok(line) => {
                    if line.chars().nth(0) == Some('-') && !lines.is_empty() {
                        assembly_record(&lines, &mut output, options.code_page)?;
                        lines.clear();
                    }
                    lines += &line;
                    lines.push('\n');
                }
            }
        }
        if !lines.is_empty() {
            assembly_record(&lines, &mut output, options.code_page)?;
        }
    }
    Ok(())
}
