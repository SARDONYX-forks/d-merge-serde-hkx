use winnow::Parser as _;
use winnow::{
    ModalResult,
    ascii::Caseless,
    combinator::{alt, delimited},
    error::{StrContext::*, StrContextValue::*},
    token::take_until,
};

/// Try to parse eventID. in `$eventID[<eventID>]$`
///
/// # Errors
/// If not found `$eventID[` `]$`
fn event_id<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    delimited(Caseless("$eventID["), take_until(0.., "]$"), "]$")
        .context(Expected(Description(
            "eventID(e.g. `$eventID[sampleEventName]$`)",
        )))
        .parse_next(input)
}

/// Try to parse variableID. in `$variableID[<variableID>]$`
///
/// # Errors
/// If not found `$variableID[` `]$`
fn variable_id<'a>(input: &mut &'a str) -> ModalResult<&'a str> {
    delimited(Caseless("$variableID["), take_until(0.., "]$"), "]$")
        .context(Expected(Description(
            "variableID(e.g. `$variableID[sampleName]$`)",
        )))
        .parse_next(input)
}

macro_rules! create_enum {
    ($($name:ident: $type:ty),+ $(,)?) => {
        $(
            #[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
            #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
            /// Represents either a primitive value, an event ID, or a variable ID.
            pub enum $name<'a> {
                /// Normal primitive value that does not require replacement.
                Number($type),
                /// Event ID pointing to the index of `eventNames` in `hkbBehaviorGraphStringData`.
                /// e.g., `$eventID[IdName]$`
                EventId(std::borrow::Cow<'a, str>),
                /// Variable ID pointing to the index of `variableNames` in `hkbBehaviorGraphStringData`.
                /// e.g., `$variableID[IdName]$`
                VariableId(std::borrow::Cow<'a, str>),
            }

            impl Default for $name<'_> {
                #[inline]
                fn default() -> Self {
                    Self::Number(Default::default())
                }
            }

            impl<'a> TryFrom<&'a str> for $name<'a> {
                type Error = String;

                fn try_from(s: &'a str) -> Result<Self, Self::Error> {
                    match <$type as crate::parse_int::ParseNumber>::parse(s) {
                        Ok(value) => return Ok(Self::Number(value)),
                        Err(_) => {
                            alt((
                                event_id.map(|n| Self::EventId(n.into())),
                                variable_id.map(|n| Self::VariableId(n.into())),
                            ))
                            .parse(s)
                            .map_err(|_| format!("Expected number/`$eventID[IdName]$`/`$variableID[IdName]$`. but got invalid string: {s}")) }
                    }
                }
            }

            impl<'a> TryFrom<String> for $name<'a> {
                type Error = String;

                fn try_from(s: String) -> Result<Self, Self::Error> {
                    use std::borrow::Cow;
                    if let Ok(num) = s.parse::<$type>() {
                        Ok(Self::Number(num))
                    } else if let Some(captures) = s.strip_prefix("$eventID[").and_then(|s| s.strip_suffix("]$")) {
                        Ok(Self::EventId(Cow::Owned(captures.to_string())))
                    } else if let Some(captures) = s.strip_prefix("$variableID[").and_then(|s| s.strip_suffix("]$")) {
                        Ok(Self::VariableId(Cow::Owned(captures.to_string())))
                    } else {
                        Err(format!("Expected `$eventID[IdName]$`/`$variableID[IdName]$`. but got invalid string: {s}"))
                    }
                }
            }

            impl<'a> ::core::fmt::Display for $name<'a> {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    match self {
                        Self::Number(n) => write!(f, "{}", n),
                        Self::EventId(e) => write!(f, "$eventID[{}]$", e),
                        Self::VariableId(v) => write!(f, "$variableID[{}]$", v),
                    }
                }
            }

            #[cfg(feature = "serde")]
            impl<'a> serde::Serialize for $name<'a> {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    match self {
                        Self::Number(n) => n.serialize(serializer),
                        Self::EventId(e) => serializer.serialize_str(&format!("$eventID[{}]$", e)),
                        Self::VariableId(v) => serializer.serialize_str(&format!("$variableID[{}]$", v)),
                    }
                }
            }

            #[cfg(feature = "serde")]
            impl<'de: 'a, 'a> serde::Deserialize<'de> for $name<'a> {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    struct ValueVisitor<'a>(core::marker::PhantomData<&'a ()>);

                    impl<'de: 'a, 'a> serde::de::Visitor<'de> for ValueVisitor<'a> {
                        type Value = $name<'a>;

                        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                            formatter.write_str("a number or an event/variable ID string")
                        }

                        fn visit_borrowed_str<E>(self, value: &'de str) -> Result<Self::Value, E>
                        where
                            E: serde::de::Error,
                        {
                            $name::try_from(value).map_err(E::custom)
                        }

                        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                        where
                            E: serde::de::Error,
                        {
                            $name::try_from(value.to_string()).map_err(E::custom)
                        }

                        fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
                        where
                            E: serde::de::Error,
                        {
                            $name::try_from(value).map_err(E::custom)
                        }

                        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
                        where
                            E: serde::de::Error,
                        {
                            Ok($name::Number(value as $type))
                        }

                        fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
                        where
                            E: serde::de::Error,
                        {
                            Ok($name::Number(value as $type))
                        }
                    }

                    deserializer.deserialize_any(ValueVisitor(core::marker::PhantomData))
                }
            }
        )*
    };
}

create_enum! [
     U8: u8,
    U16: u16,
    U32: u32,
    U64: u64,
     I8: i8,
    I16: i16,
    I32: i32,
    I64: i64,
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_str_number() {
        assert_eq!(U32::try_from("42"), Ok(U32::Number(42)));
        assert_eq!(I16::try_from("-10"), Ok(I16::Number(-10)));
    }

    #[test]
    fn test_from_str_event() {
        assert_eq!(
            U32::try_from("$eventID[Start]$"),
            Ok(U32::EventId("Start".into()))
        );
        assert_eq!(
            U32::try_from("$eventID[Jump]$"),
            Ok(U32::EventId("Jump".into()))
        );
    }

    #[test]
    fn test_from_str_variable() {
        assert_eq!(
            U32::try_from("$variableID[Health]$"),
            Ok(U32::VariableId("Health".into()))
        );
        assert_eq!(
            U32::try_from("$variableID[Stamina]$"),
            Ok(U32::VariableId("Stamina".into()))
        );
    }

    #[test]
    fn test_from_str_invalid() {
        assert!(U32::try_from("random_text").is_err());
        assert!(U32::try_from("$invalidID[Oops]$").is_err());
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialize() {
        let num = U32::Number(123);
        assert_eq!(serde_json::to_string(&num).unwrap(), "123");

        let event = U32::EventId("Fire".into());
        assert_eq!(
            serde_json::to_string(&event).unwrap(),
            "\"$eventID[Fire]$\""
        );

        let var = U32::VariableId("Health".into());
        assert_eq!(
            serde_json::to_string(&var).unwrap(),
            "\"$variableID[Health]$\""
        );
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_deserialize() {
        let json = "123";
        let val: U32 = serde_json::from_str(json).unwrap();
        assert_eq!(val, U32::Number(123));

        let json = "\"$eventID[Jump]$\"";
        let event: U32 = serde_json::from_str(json).unwrap();
        assert_eq!(event, U32::EventId("Jump".into()));

        let json = "\"$variableID[Speed]$\"";
        let var: U32 = serde_json::from_str(json).unwrap();
        assert_eq!(var, U32::VariableId("Speed".into()));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_invalid_deserialize() {
        let json = "\"invalid_text\"";
        assert!(serde_json::from_str::<U32>(json).is_err());
    }
}
