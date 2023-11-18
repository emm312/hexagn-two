use crate::ast::Type;

pub fn mangle_function(name: &String, args: &Vec<(Type, String)>, ret_t: &Type) -> String {
    format!(
        "_Hx{}{}{}{}",
        name.len(),
        name,
        ret_t,
        args.iter()
            .map(|(t, _)| t)
            .fold(String::new(), |acc, t| format!("{}{}", acc, t))
    )
}
