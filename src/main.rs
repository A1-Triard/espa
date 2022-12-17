#![feature(drain_filter)]
#![feature(once_cell)]

#![deny(warnings)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::question_mark)]
#![allow(clippy::transmute_ptr_to_ptr)]
#![allow(clippy::type_complexity)]

use clap::{Arg, ArgAction, ArgMatches, Command, value_parser};
use clap::builder::PossibleValuesParser;
use either::{Either, Left, Right};
use esl::*;
use esl::code::*;
use esl::read::*;
use indoc::indoc;
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use regex::Regex;
use std::env::current_exe;
use std::ffi::{OsStr, OsString};
use std::fs::{File, remove_file, rename};
use std::io::{Write, stdout, stdin, BufRead, BufReader, BufWriter};
use std::mem::transmute;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::str::{self, FromStr};
use std::sync::LazyLock;
use uuid::Uuid;

#[cfg(target_os = "windows")]
const DEFAULT_NEWLINE: &str = "dos";
#[cfg(not(target_os = "windows"))]
const DEFAULT_NEWLINE: &str = "unix";

struct Options {
    exclude_records: Vec<Tag>,
    exclude_fields: Vec<(Option<Tag>, Option<Tag>, Tag)>,
    include_records: Vec<Tag>,
    include_fields: Vec<(Option<Tag>, Option<Tag>, Tag)>,
    fit: bool,
    keep: Option<bool>,
    disassemble: Option<&'static str>,
    verbose: bool,
    code_page: CodePage,
}

impl Options {
    fn skip_record(&self, record_tag: Tag) -> bool {
        (!self.include_records.is_empty() && !self.include_records.contains(&record_tag)) ||
        self.exclude_records.contains(&record_tag)
    }

    fn skip_field(&self, record_tag: Tag, prev_tag: Tag, field_tag: Tag) -> bool {
        let predicate = |(r, p, f): &(Option<Tag>, Option<Tag>, Tag)|
            *f == field_tag && p.as_ref().map_or(true, |&p| p == prev_tag) && r.as_ref().map_or(true, |&r| r == record_tag)
        ;
        (!self.include_fields.is_empty() && !self.include_fields.iter().any(predicate)) ||
        self.exclude_fields.iter().any(predicate)
    }
    
    fn convert(&self, record: &mut Record) {
        let record_tag = record.tag;
        let mut prev_tag = META;
        record.fields.drain_filter(|(field_tag, field)| {
            let res = if self.skip_field(record_tag, prev_tag, *field_tag) {
                true
            } else {
                if self.fit {
                    field.fit(record_tag, prev_tag, *field_tag);
                }
                false
            };
            prev_tag = *field_tag;
            res
        });
    }
}

fn parse_tag(value: &str) -> Result<Option<Tag>, String> {
    Ok(if value.is_empty() {
        None
    } else {
        Some(Tag::from_str(value).map_err(|()| format!("invalid tag {value:?}"))?)
    })
}

fn parse_cond(value: &str) -> Result<Either<Tag, (Option<Tag>, Option<Tag>, Tag)>, String> {
    let mut tags = value.split(':');
    let record_tag = tags.next().ok_or_else(|| format!("invalid COND {value:?}"))?;
    let (prev_tag, field_tag) = if let Some(prev_tag) = tags.next() {
        let field_tag = tags.next().ok_or_else(|| format!("invalid COND {value:?}"))?;
        if tags.next().is_some() { return Err(format!("invalid COND {value:?}")); }
        (prev_tag, field_tag)
    } else {
        ("", "")
    };
    let record_tag = parse_tag(record_tag)?;
    let prev_tag = parse_tag(prev_tag)?;
    let field_tag = parse_tag(field_tag)?;
    if let Some(field_tag) = field_tag {
        Ok(Right((record_tag, prev_tag, field_tag)))
    } else {
        let record_tag = record_tag.ok_or_else(|| format!("invalid COND {value:?}"))?;
        if prev_tag.is_some() { return Err(format!("invalid COND {value:?}")); }
        Ok(Left(record_tag))
    }
}

fn parse_conds(args: &ArgMatches, name: &'static str) -> (Vec<Tag>, Vec<(Option<Tag>, Option<Tag>, Tag)>) {
    let mut records = Vec::new();
    let mut fields = Vec::new();
    if let Some(values) = args.get_many::<String>(name) {
        for value in values {
            match parse_cond(value) {
                Ok(Left(record_tag)) => records.push(record_tag),
                Ok(Right(field_cond)) => fields.push(field_cond),
                Err(e) => {
                    eprintln!("error: {e}\n\nFor more information try --help");
                    exit(1);
                }
            }
        }
    }
    (records, fields)
}

const HYPHEN: &OsStr = unsafe { transmute("-") };

fn parse_args() -> (Options, Vec<Option<PathBuf>>) {
    let app = current_exe().ok()
        .and_then(|x| x.file_stem().map(|x| x.to_os_string()))
        .and_then(|x| x.into_string().ok())
        .map(|x| Box::leak(x.into_boxed_str()) as &str)
        .unwrap_or("espa");
    let args = Command::new(app)
        .version(env!("CARGO_PKG_VERSION"))
        .disable_colored_help(true)
        .help_template("Usage: {usage}\n\n{about}\n\n{options}{after-help}")
        .after_help(indoc!("
            <COND> can be in one of the following forms:
                RECORD_TAG
                RECORD_TAG::FIELD_TAG
                ::FIELD_TAG
                RECORD_TAG:PREV_TAG:FIELD_TAG
                :PREV_TAG:FIELD_TAG

            When FILE is -, read standard input.

            Report bugs to <internalmike@gmail.com> (in English or Russian).
        "))
        .about("Convert FILEs from the .esm/.esp/.ess format to YAML and back.")
        .disable_help_flag(true)
        .arg(Arg::new("help")
            .short('h')
            .long("help")
            .help("display this help and exit")
            .action(ArgAction::Help)
        )
        .arg(Arg::new("FILE")
            .action(ArgAction::Append)
            .value_parser(value_parser!(OsString))
        )
        .arg(Arg::new("disassemble")
            .short('d')
            .long("disassemble")
            .help("convert binary .es{s,p,m} file to YAML")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("verbose")
            .short('v')
            .long("verbose")
            .help("verbose mode")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("fit")
            .short('f')
            .long("fit")
            .help("remove redundant trailing zeros and other garbage")
            .action(ArgAction::SetTrue)
        )
        .disable_version_flag(true)
        .arg(Arg::new("version")
            .short('V')
            .long("version")
            .help("display the version number and exit")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("exclude")
            .short('e')
            .long("exclude")
            .action(ArgAction::Append)
            .value_name("COND")
            .help("skip specified records/fields")
        )
        .arg(Arg::new("include")
            .short('i')
            .long("include")
            .action(ArgAction::Append)
            .value_name("COND")
            .help("skip all but specified records/fields")
        )
        .arg(Arg::new("code_page")
            .short('p')
            .long("code-page")
            .value_name("LANG")
            .value_parser(PossibleValuesParser::new([
                "en",
                "ru",
                "un",
            ]))
            .required_unless_present("version")
            .help("text code page")
        )
        .arg(Arg::new("keep")
            .short('k')
            .long("keep")
            .help("keep (don't delete) input files")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("use_stdout")
            .short('c')
            .long("stdout")
            .conflicts_with("keep")
            .help("write on standard output, keep original files unchanged")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("newline")
            .short('n')
            .long("newline")
            .value_name("NL")
            .default_value(DEFAULT_NEWLINE)
            .value_parser(PossibleValuesParser::new([
                "unix",
                "dos",
            ]))
            .requires("disassemble")
            .help("newline style")
        )
        .dont_collapse_args_in_usage(true)
        .get_matches()
    ;
    if *args.get_one("version").unwrap() {
        println!(env!("CARGO_PKG_VERSION"));
        exit(0);
    }
    let files = args.get_many::<OsString>("FILE").map_or_else(Vec::new, |v| v.map(|v| if v == HYPHEN {
        None
    } else {
        Some(PathBuf::from(v))
    }).collect());
    let fit = *args.get_one("fit").unwrap();
    let keep = if *args.get_one("use_stdout").unwrap() {
        None
    } else {
        Some(*args.get_one("keep").unwrap())
    };
    let disassemble = if *args.get_one("disassemble").unwrap() {
        Some(match args.get_one::<String>("newline").unwrap().as_ref() {
            "dos" => "\r\n",
            "unix" => "\n",
            _ => unreachable!()
        })
    } else {
        None
    };
    let verbose = *args.get_one("verbose").unwrap();
    let code_page = match args.get_one::<String>("code_page").unwrap().as_ref() {
        "en" => CodePage::English,
        "ru" => CodePage::Russian,
        "un" => CodePage::Unicode,
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
    let mut errors = false;
    for file in files.iter() {
        if let Err(e) = convert_file(file.as_ref().map(|x| x.as_path()), &options) {
            errors = true;
            eprintln!("{e}.");
        }
    }
    if errors { exit(2) }
}

const YAML_SUFFIX: &OsStr = unsafe { transmute("yaml") };
const DOT: &OsStr = unsafe { transmute(".") };
static ESL_SUFFIXES: &[&OsStr] = &[
    unsafe { transmute("ess") }, 
    unsafe { transmute("ESS") },
    unsafe { transmute("esp") },
    unsafe { transmute("ESP") },
    unsafe { transmute("esm") },
    unsafe { transmute("ESM") },
    unsafe { transmute("omwsave") },
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
    let mut output_file_name: Vec<u8> = Vec::with_capacity(input_file_name.len() + DOT.len() + YAML_SUFFIX.len());
    output_file_name.extend_from_slice(unsafe { transmute(input_file_name) });
    output_file_name.extend_from_slice(unsafe { transmute(DOT) });
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
        get_disassembled_name(input_name)
    } else {
        get_assembled_name(input_name)
    }
}

fn display(out: bool, path: Option<&Path>) -> String {
    if let Some(path) = path {
        format!("{}", path.display())
    } else {
        if out { "<stdout>" } else { "<stdin>" }.into()
    }
}

fn convert_file(input_name: Option<&Path>, options: &Options) -> Result<(), String> {
    let output_name = if let Some(input_name) = input_name {
        let output_name = get_output_name(input_name, options.disassemble.is_some())?;
        if options.keep.is_none() { None } else { Some(output_name) }
    } else {
        None
    };
    if options.verbose {
        eprintln!("{} -> {}", display(false, input_name), display(true, output_name.as_deref()));
    }
    let mut temp_name = None;
    let res = if let Err(e) = convert_records(input_name, output_name.as_deref(), options, &mut temp_name) {
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

static BASE_64_FIELD_PATTERN: LazyLock<Regex> = LazyLock::new(|| Regex::new(
    r"^- ([0-9A-Za-z][0-9A-Za-z][0-9A-Za-z][0-9A-Za-z]): ([0-9A-Za-z+/=]+)$"
).unwrap());

const BASE_64_FIELD_LINE_LIMIT: usize = 112;

fn format_record_yaml(s: &str, newline: &str) -> String {
    let mut res = String::with_capacity(s.len()); // at least
    res.push_str("- ");
    for (is_first, line) in s.trim_end_matches('\n').split('\n').identify_first() {
        if !is_first { res.push_str("  "); }
        let formatted = if line.len() > BASE_64_FIELD_LINE_LIMIT {
            if let Some(captures) = BASE_64_FIELD_PATTERN.captures(line) {
                res.push_str("- ");
                res.push_str(captures.get(1).unwrap().as_str());
                res.push_str(": \"");
                let value = captures.get(2).unwrap().as_str();
                for (is_first, is_last, chunk) in value.as_bytes().chunks(BASE_64_FIELD_LINE_LIMIT).identify_first_last() {
                    if !is_first { res.push_str("           "); }
                    res.push_str(str::from_utf8(chunk).unwrap());
                    res.push(if is_last { '\"' } else { '\\' });
                    res.push_str(newline);
                }
                true
            } else {
                false
            }
        } else {
            false
        };
        if !formatted {
            res.push_str(line);
            res.push_str(newline);
        }
    }
    res
}

fn convert_records(input_name: Option<&Path>, output_name: Option<&Path>, options: &Options, temp_name: &mut Option<PathBuf>)
    -> Result<(), String> {
    
    let stdin = stdin();
    let stdout = stdout();
    let (mut input, mut output): (Box<dyn BufRead>, Box<dyn Write>) =
        if let Some(input_name) = input_name {
            let input = Box::new(BufReader::new(File::open(input_name).map_err(|e| format!("{}: {}", input_name.display(), e))?));
            if let Some(output_name) = output_name {
                let temp = input_name.with_file_name(OsString::from(format!("{}", Uuid::new_v4().simple())));
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
        let records = Records::new(
            options.code_page,
            if options.fit { RecordReadMode::Lenient } else { RecordReadMode::Strict }, 0, &mut input
        );
        let records = records.filter(|record| match record {
            Ok(record) => !options.skip_record(record.tag),
            Err(_) => true,
        });
        for (is_first, record) in records.identify_first() {
            match record {
                Err(e) => return Err(format!("{}: {}", display(false, input_name), e)),
                Ok(mut record) => {
                    options.convert(&mut record);
                    let record = serde_yaml::to_string(&record).unwrap();
                    if !is_first { write!(output, "{newline}").map_err(|e| format!("{e}"))?; }
                    write!(output, "{}", format_record_yaml(&record, newline)).map_err(|e| format!("{e}"))?;
                }
            }
        }
    } else {
        fn assembly_record(lines: &str, output: &mut dyn Write, options: &Options) -> Result<(), String> {
            let records: Vec<Record> = serde_yaml::from_str(lines).map_err(|e| format!("{e}"))?;
            for mut record in records.into_iter().filter(|x| !options.skip_record(x.tag)) {
                options.convert(&mut record);
                serialize_into(&record, output, options.code_page, false).map_err(|e| format!("{e}"))?;
            }
            Ok(())
        }
        let mut lines = String::with_capacity(128);
        for line in input.lines() {
            match line {
                Err(e) => return Err(format!("{}: {}", display(false, input_name), e)),
                Ok(line) => {
                    if line.starts_with('-') && !lines.is_empty() {
                        assembly_record(&lines, &mut output, options)?;
                        lines.clear();
                    }
                    lines += &line;
                    lines.push('\n');
                }
            }
        }
        if !lines.is_empty() {
            assembly_record(&lines, &mut output, options)?;
        }
    }
    Ok(())
}
