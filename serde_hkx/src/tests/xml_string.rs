//! XML string values must keep leading/trailing whitespace.
//!
//! Vanilla SkyrimSE `magicbehavior.hkx` has `" iState_NPCSneaking"` in `hkbBehaviorGraphStringData.variableNames`.
use super::ClassMap;
use crate::{HavokSort as _, bytes::serde::hkx_header::HkxHeader, from_str, to_string};
use havok_classes::Classes;

const XML: &str = r###"<?xml version="1.0" encoding="ascii"?>
<hkpackfile classversion="8" contentsversion="hk_2010.2.0-r1" toplevelobject="#0001">

	<hksection name="__data__">

		<hkobject name="#0001" class="hkbBehaviorGraphStringData" signature="0xc713064e">
			<hkparam name="eventNames" numelements="0"></hkparam>
			<hkparam name="attributeNames" numelements="0"></hkparam>
			<hkparam name="variableNames" numelements="3">
				<hkcstring> iState_NPCSneaking</hkcstring>
				<hkcstring>	tab_and_trailing </hkcstring>
				<hkcstring>normal</hkcstring>
			</hkparam>
			<hkparam name="characterPropertyNames" numelements="0"></hkparam>
		</hkobject>

	</hksection>

</hkpackfile>
"###;

#[test]
fn should_keep_whitespace_of_strings() {
    let mut classes: ClassMap = from_str(XML).unwrap_or_else(|e| panic!("{e}"));

    let Some(Classes::hkbBehaviorGraphStringData(data)) = classes.get("#0001") else {
        panic!("Not found hkbBehaviorGraphStringData");
    };
    let names: Vec<_> = data
        .m_variableNames
        .iter()
        .map(|s| s.get_ref().as_deref().unwrap_or_default().to_string())
        .collect();
    assert_eq!(names, [" iState_NPCSneaking", "\ttab_and_trailing ", "normal"]);

    // XML -> classes -> XML must be the same strings.
    let top_ptr = classes.sort_for_xml().unwrap();
    let xml = to_string(&classes, &top_ptr, &HkxHeader::new_skyrim_se()).unwrap();
    assert!(xml.contains("<hkcstring> iState_NPCSneaking</hkcstring>"), "{xml}");
    assert!(xml.contains("<hkcstring>\ttab_and_trailing </hkcstring>"), "{xml}");
}
