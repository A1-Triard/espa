#![deny(warnings)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::question_mark)]
#![allow(clippy::transmute_ptr_to_ptr)]
#![allow(clippy::type_complexity)]

use clap::{Arg, ArgAction, ArgMatches, Command, value_parser};
use clap::builder::PossibleValuesParser;
use esl::*;
use esl::code::{self};
use esl::read::*;
use indoc::indoc;
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use regex::Regex;
use serde::de::DeserializeSeed;
use serde_serialize_seed::{ValueWithSeed, VecSerde};
use std::env::current_exe;
use std::ffi::{OsStr, OsString};
use std::fmt::{self, Display, Formatter};
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
    allow_blank_records: bool,
    exclude_records: Vec<Tag>,
    exclude_fields: Vec<(Option<Tag>, Option<Tag>, Tag)>,
    include_records: Vec<Tag>,
    include_fields: Vec<(Option<Tag>, Option<Tag>, Tag)>,
    fit: bool,
    keep: Option<bool>,
    disassemble: Option<&'static str>,
    verbose: bool,
    code_page: CodePage,
    omwsave: bool,
}

impl Options {
    fn is_blank_record(&self, record: &Record) -> bool {
        record.flags.is_empty() && record.fields.is_empty()
    }

    fn records_filter(&self, record_tag: Tag) -> bool {
        (self.include_records.is_empty() || self.include_records.contains(&record_tag))
            && !self.exclude_records.contains(&record_tag)
    }

    fn fields_filter(&self, record_tag: Tag, prev_tag: Tag, field_tag: Tag) -> bool {
        let predicate = |&(ref r, ref p, f): &(Option<Tag>, Option<Tag>, Tag)| {
            f == field_tag
                && p.as_ref().map_or(true, |&p| p == prev_tag)
                && r.as_ref().map_or(true, |&r| r == record_tag)
        };
        (self.include_fields.is_empty() || self.include_fields.iter().any(predicate))
            && !self.exclude_fields.iter().any(predicate)
    }

    fn convert(&self, mut record: Record, omwsave: bool) -> Option<Result<Record, String>> {
        let record_tag = record.tag;
        if !self.allow_blank_records && self.is_blank_record(&record) {
            return Some(Err(
                format!("error: blank record '{record_tag}'\n\nTo allow records with no fields, use --allow-blank-records")
            ));
        }
        if !self.records_filter(record_tag) { return None; }
        let mut prev_tag = META;
        record.fields.retain(|&(field_tag, _)| {
            let keep = self.fields_filter(record_tag, prev_tag, field_tag);
            prev_tag = field_tag;
            keep
        });
        if self.fit {
            let mut prev_tag = META;
            for &mut (field_tag, ref mut field) in &mut record.fields {
                field.fit(record_tag, prev_tag, field_tag, omwsave);
                prev_tag = field_tag;
            }
        }
        if !self.fields_filter(record_tag, META, META) {
            record.flags = RecordFlags::empty();
        }
        if !self.allow_blank_records && self.is_blank_record(&record) {
            None
        } else {
            Some(Ok(record))
        }
    }
}

fn parse_tag(value: &str) -> Tag {
    Tag::from_str(value).unwrap_or_else(|()| {
        eprintln!("Error: invalid tag {value:?}");
        exit(1);
    })
}

fn parse_record_tags(args: &ArgMatches, name: &'static str) -> Vec<Tag> {
    let mut records = Vec::new();
    if let Some(values) = args.get_many::<String>(name) {
        for value in values {
            records.push(parse_tag(value));
        }
    }
    records
}

fn parse_field_cond(value: &str) -> Result<(Option<Tag>, Option<Tag>, Tag), ()> {
    let mut tags = value.split(':');
    let a = tags.next().ok_or(())?;
    if let Some(b) = tags.next() {
        let c = tags.next().ok_or(())?;
        if tags.next().is_some() { return Err(()); }
        let record_tag = if a.is_empty() { None } else { Some(parse_tag(a)) };
        let prev_tag = if b.is_empty() { None } else { Some(parse_tag(b)) };
        let field_tag = parse_tag(c);
        Ok((record_tag, prev_tag, field_tag))
    } else {
        Ok((None, None, parse_tag(a)))
    }
}

fn parse_field_conds(args: &ArgMatches, name: &'static str) -> Vec<(Option<Tag>, Option<Tag>, Tag)> {
    let mut fields = Vec::new();
    if let Some(values) = args.get_many::<String>(name) {
        for value in values {
            let field_cond = parse_field_cond(value).unwrap_or_else(|()| {
                eprintln!("Error: invalid FIELD_COND {value:?}");
                eprintln!();
                eprintln!("For more information try --help");
                exit(1);
            });
            fields.push(field_cond);
        }
    }
    fields
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
            <FIELD_COND> can be in one of the following forms:
                FIELD_TAG
                RECORD_TAG:PREV_TAG:FIELD_TAG
                RECORD_TAG::FIELD_TAG
                :PREV_TAG:FIELD_TAG
                ::FIELD_TAG

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
        .arg(Arg::new("omwsave")
            .short('o')
            .long("omwsave")
            .help("use .omwsave format")
            .action(ArgAction::SetTrue)
        )
        .disable_version_flag(true)
        .arg(Arg::new("version")
            .short('V')
            .long("version")
            .help("display the version number and exit")
            .action(ArgAction::SetTrue)
        )
        .arg(Arg::new("exclude_records")
            .short('E')
            .long("exclude-records")
            .action(ArgAction::Append)
            .value_name("RECORD_TAG")
            .help("skip specified records")
        )
        .arg(Arg::new("include_records")
            .short('I')
            .long("include-records")
            .action(ArgAction::Append)
            .value_name("RECORD_TAG")
            .help("skip all but specified records")
        )
        .arg(Arg::new("exclude_fields")
            .short('e')
            .long("exclude-fields")
            .action(ArgAction::Append)
            .value_name("FIELD_COND")
            .help("skip specified fields")
        )
        .arg(Arg::new("include_fields")
            .short('i')
            .long("include-fields")
            .action(ArgAction::Append)
            .value_name("FIELD_COND")
            .help("skip all but specified fields")
        )
        .arg(Arg::new("allow_blank_records")
            .short('b')
            .long("allow-blank-records")
            .action(ArgAction::SetTrue)
            .conflicts_with("exclude_fields")
            .conflicts_with("include_fields")
            .help("allow records with no fields")
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
    let omwsave = *args.get_one("omwsave").unwrap();
    let verbose = *args.get_one("verbose").unwrap();
    let code_page = match args.get_one::<String>("code_page").unwrap().as_ref() {
        "en" => CodePage::English,
        "ru" => CodePage::Russian,
        "un" => CodePage::Unicode,
        _ => unreachable!()
    };
    let allow_blank_records = *args.get_one("allow_blank_records").unwrap();
    let exclude_records = parse_record_tags(&args, "exclude_records");
    let include_records = parse_record_tags(&args, "include_records");
    let exclude_fields = parse_field_conds(&args, "exclude_fields");
    let include_fields = parse_field_conds(&args, "include_fields");
    (Options {
        fit, keep, disassemble, verbose, code_page, allow_blank_records,
        exclude_records, exclude_fields, include_records, include_fields,
        omwsave,
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

struct FileArg<'a> {
    out: bool,
    path: Option<&'a Path>,
}

impl<'a> Display for FileArg<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(path) = self.path {
            write!(f, "{}", path.display())
        } else if self.out {
            write!(f, "<stdout>")
        } else {
            write!(f, "<stdin>")
        }
    }
}

fn file_err<E: Display>(out: bool, path: Option<&Path>, e: E) -> String {
    format!("{}: {}", FileArg { out, path }, e)
}

fn convert_file(input_name: Option<&Path>, options: &Options) -> Result<(), String> {
    let output_name = if let Some(input_name) = input_name {
        let output_name = get_output_name(input_name, options.disassemble.is_some())?;
        if options.keep.is_none() { None } else { Some(output_name) }
    } else {
        None
    };
    if options.verbose {
        eprintln!(
            "{} -> {}",
            FileArg { out: false, path: input_name },
            FileArg { out: true, path: output_name.as_deref() }
        );
    }
    let mut temp_name = None;
    let res = if let Err(e) = convert_records(input_name, output_name.as_deref(), options, &mut temp_name) {
        Err(e)
    } else {
        if let Some(temp_name) = &temp_name {
            let output_file = output_name.as_ref().unwrap();
            let output_file = output_file.as_path();
            rename(temp_name, output_file).map_err(|e| file_err(true, output_name.as_deref(), e))
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
        let input_file = input_name.unwrap();
        remove_file(input_file).map_err(|e| file_err(false, input_name, e))
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

fn convert_records(
    input_name: Option<&Path>,
    output_name: Option<&Path>,
    options: &Options,
    temp_name: &mut Option<PathBuf>
) -> Result<(), String> {
    let stdin = stdin();
    let stdout = stdout();
    let (mut input, mut output): (Box<dyn BufRead>, Box<dyn Write>) =
        if let Some(input_name) = input_name {
            let input = Box::new(BufReader::new(File::open(input_name).map_err(|e| file_err(false, Some(input_name), e))?));
            if let Some(output_name) = output_name {
                let temp = input_name.with_file_name(OsString::from(format!("{}", Uuid::new_v4().simple())));
                let output =
                    Box::new(BufWriter::new(File::create(&temp).map_err(|e| file_err(true, Some(output_name), e))?));
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
            if options.fit { RecordReadMode::Lenient } else { RecordReadMode::Strict }, options.omwsave, 0, &mut input
        );
        let records = records.filter_map(|record| match record {
            Ok(record) => options.convert(record, options.omwsave),
            Err(e) => Some(Err(file_err(false, input_name, e))),
        });
        for (is_first, record) in records.identify_first() {
            match record {
                Err(e) => return Err(file_err(false, input_name, e)),
                Ok(record) => {
                    let record = serde_yaml::to_string(&ValueWithSeed(&record, RecordSerde { code_page: None, omwsave: options.omwsave })).unwrap();
                    if !is_first { write!(output, "{newline}").map_err(|e| file_err(true, output_name, e))?; }
                    write!(output, "{}", format_record_yaml(&record, newline)).map_err(|e| file_err(true, output_name, e))?;
                }
            }
        }
    } else {
        let mut assembly_record = |lines: &str| {
            let records = serde_yaml::Deserializer::from_str(lines);
            let records = VecSerde(RecordSerde { code_page: None, omwsave: options.omwsave }).deserialize(records)
                .map_err(|e| file_err(false, input_name, e))?;
            for record in records.into_iter().filter_map(|x| options.convert(x, options.omwsave)) {
                match record {
                    Err(e) => return Err(file_err(false, input_name, e)),
                    Ok(record) =>
                        code::serialize_into(
                            &ValueWithSeed(&record, RecordSerde { code_page: Some(options.code_page), omwsave: options.omwsave }), &mut output, false
                        ).map_err(|e| file_err(true, output_name, e))?
                }
            }
            Ok(())
        };
        let mut lines = String::with_capacity(128);
        for line in input.lines() {
            match line {
                Err(e) => return Err(file_err(false, input_name, e)),
                Ok(line) => {
                    if line.starts_with('-') && !lines.is_empty() {
                        assembly_record(&lines)?;
                        lines.clear();
                    }
                    lines += &line;
                    lines.push('\n');
                }
            }
        }
        if !lines.is_empty() {
            assembly_record(&lines)?;
        }
    }
    Ok(())
}
