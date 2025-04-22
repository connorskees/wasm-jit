use fallible_iterator::FallibleIterator;
use gimli::{
    Attribute, DW_AT_name, DebugAddr, DebugStrOffset, DebuggingInformationEntry, DwAt, DwTag,
    EndianSlice, LittleEndian,
};
use wasm_jit::{CustomSection, ImportValue, Interpreter, Module, Value, WResult};

fn main() -> WResult<()> {
    let buffer = std::fs::read(
        // "/root/wasm-jit/files/array-contains.wasm",
        "/root/wasm-jit/test-project/target/wasm32-unknown-unknown/debug/test_project.wasm",
    )
    .unwrap();
    // let buffer = std::fs::read("./foo.wasm").unwrap();

    let module = Module::new(&buffer)?;

    // [src/parse.rs:264:9] name = ".debug_abbrev"
    // [src/parse.rs:264:9] name = ".debug_info"
    // [src/parse.rs:264:9] name = ".debug_ranges"
    // [src/parse.rs:264:9] name = ".debug_str"
    // [src/parse.rs:264:9] name = ".debug_line"
    // [src/parse.rs:264:9] name = "name"
    // [src/parse.rs:264:9] name = "producers"
    // [src/parse.rs:264:9] name = "target_features"

    // dbg!(module.custom_sections());

    // let custom_sections = module.custom_sections();

    let abbr = module.custom_section_with_name(".debug_abbrev").unwrap();
    let info = module.custom_section_with_name(".debug_info").unwrap();
    let line = module.custom_section_with_name(".debug_line").unwrap();
    let ranges = module.custom_section_with_name(".debug_ranges").unwrap();
    let string = module.custom_section_with_name(".debug_str").unwrap();

    let abbr = gimli::read::DebugAbbrev::new(abbr, LittleEndian);
    let info = gimli::read::DebugInfo::new(info, LittleEndian);
    let ranges = gimli::read::DebugRanges::new(ranges, LittleEndian);
    let line = gimli::read::DebugLine::new(line, LittleEndian);
    let string = gimli::read::DebugStr::new(string, LittleEndian);

    let units = info.units().collect::<Vec<_>>().unwrap();

    let load = |a| -> Result<_, ()> {
        Ok(match a {
            gimli::SectionId::DebugAbbrev => Empty,
            gimli::SectionId::DebugAddr => z,
            gimli::SectionId::DebugAranges => todo!(),
            gimli::SectionId::DebugCuIndex => todo!(),
            gimli::SectionId::DebugFrame => todo!(),
            gimli::SectionId::EhFrame => todo!(),
            gimli::SectionId::EhFrameHdr => todo!(),
            gimli::SectionId::DebugInfo => Ok(info),
            gimli::SectionId::DebugLine => todo!(),
            gimli::SectionId::DebugLineStr => todo!(),
            gimli::SectionId::DebugLoc => todo!(),
            gimli::SectionId::DebugLocLists => todo!(),
            gimli::SectionId::DebugMacinfo => todo!(),
            gimli::SectionId::DebugMacro => todo!(),
            gimli::SectionId::DebugPubNames => todo!(),
            gimli::SectionId::DebugPubTypes => todo!(),
            gimli::SectionId::DebugRanges => todo!(),
            gimli::SectionId::DebugRngLists => todo!(),
            gimli::SectionId::DebugStr => todo!(),
            gimli::SectionId::DebugStrOffsets => todo!(),
            gimli::SectionId::DebugTuIndex => todo!(),
            gimli::SectionId::DebugTypes => todo!(),
        })
    };
    gimli::Dwarf::load(load);

    // let mut iter = info.units();
    // while let Some(header) = iter.next()? {
    //     // Parse the abbreviations and other information for this compilation unit.
    //     let unit = dwarf.unit(header)?;

    //     // Iterate over all of this compilation unit's entries.
    //     let mut entries = unit.entries();
    //     while let Some((_, entry)) = entries.next_dfs()? {
    //         // If we find an entry for a function, print it.
    //         if entry.tag() == gimli::DW_TAG_subprogram {
    //             println!("Found a function: {:?}", entry);
    //         }
    //     }
    // }

    // for u in units {
    //     let abbreviations = u.abbreviations(&abbr).unwrap();
    //     let mut e = u.entries(&abbreviations);

    //     while let Some((_, die)) = e.next_dfs()? {
    //         // let die: &DebuggingInformationEntry<'static, 'static, EndianSlice<'_, LittleEndian>> = die;
    //         let tag: DwTag = die.tag();
    //         // dbg!(tag);
    //         if tag.static_string() == Some("DW_TAG_formal_parameter") {
    //             if let Some(attr) = die.attr(DW_AT_name).unwrap() {
    //                 let attr: Attribute<_> = attr;
    //                 // attr.value()
    //                 dbg!(String::from_utf8_lossy(
    //                     attr.value().string_value(&string).unwrap().slice()
    //                 ));
    //                 // if let Some(val) = attr.value().string_value(&string) .offset_value() {
    //                 //     let off = DebugStrOffset(val);
    //                 //     dbg!(string.get_str(off).unwrap());
    //                 // }
    //                 // dbg!();
    //             }
    //             // dbg!(die.attrs().collect::<Vec<_>>().unwrap());
    //         }
    //     }

    //     // dbg!(u.abbreviations(&abbr).unwrap());
    //     // if let Some(f) = u.abbreviations(&abbr).unwrap().get(20) {
    //     //     dbg!(f);
    //     //     u.
    //     // }
    // }

    // dbg!(units[1].type_());

    // let mut interpreter = Interpreter::new(
    //     &buffer,
    //     &[
    //         ("env", "__linear_memory", ImportValue::Mem(vec![0; 5000])),
    //         ("env", "__indirect_function_table", ImportValue::Table(())),
    //     ],
    // )?;

    // // for i in -1000..1000 {
    // assert_eq!(
    //     interpreter
    //         .invoke_export("nested_br", &[Value::I64(5)])
    //         .unwrap()
    //         .unwrap(),
    //     Value::I64(1),
    //     // "{i}"
    // );
    // }

    Ok(())
}
