use crate::symbol_table::Type;

#[test]
fn test_type_hash_and_eq() {
    let mux_one = Type::new("Mux".into(), vec![
        Type::new("Int".into(), vec![]),
        Type::new("Str".into(), vec![]),
    ]);
    let mux_two = Type::new("Mux".into(), vec![
        Type::new("Int".into(), vec![]),
        Type::new("Str".into(), vec![]),
    ]);

    assert_eq!(mux_one, mux_two);

    let mux_one = Type::new("Mux".into(), vec![
        Type::new("Str".into(), vec![]),
        Type::new("Int".into(), vec![]),
    ]);
    let mux_two = Type::new("Mux".into(), vec![
        Type::new("Int".into(), vec![]),
        Type::new("Str".into(), vec![]),
    ]);

    assert_ne!(mux_one, mux_two);
}

//TODO: test commands checks