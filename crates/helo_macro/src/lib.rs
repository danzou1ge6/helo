use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_derive(Emit)]
pub fn derive_emit(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse_macro_input!(input);

    let data_ident = ast.ident;
    if let syn::Data::Enum(data_enum) = ast.data {
        let arms = data_enum
            .variants
            .into_iter()
            .map(|variant| {
                let variant_ident = variant.ident;
                let fields_type = variant
                    .fields
                    .into_iter()
                    .map(|field| field.ty)
                    .collect::<Vec<_>>();

                let fields_bind = (0..fields_type.len())
                    .map(|i| format_ident!("x{}", i))
                    .collect::<Vec<_>>();
                let op_code_name =
                    format_ident!("{}", camel_to_upper_snake(&variant_ident.to_string()));

                quote! {
                    #variant_ident( #(#fields_bind),* ) => writer
                        .push(#op_code_name)
                        #( .push(#fields_bind) )*
                        .finish()
                }
            })
            .collect::<Vec<_>>();

        quote! {
            impl #data_ident {
                pub fn emit(self, chunk: &mut Chunk) {
                    use #data_ident::*;
                    use OpCode::*;

                    let writer = chunk.writer();
                    match self {
                        #( #arms ),*
                    }
                }
            }
        }
        .into()
    } else {
        panic!("only enum is supported")
    }
}

fn camel_to_upper_snake(from: &str) -> String {
    let mut r = String::new();

    let mut chars = from.chars();

    r.push_str(&chars.next().unwrap().to_uppercase().to_string());

    for char in chars {
        if char.is_uppercase() {
            r.push_str("_");
            r.push(char);
        } else {
            r.push_str(&char.to_uppercase().to_string())
        }
    }

    r
}

fn camel_to_lower_snake(from: &str) -> String {
    let mut r = String::new();

    let mut chars = from.chars();

    r.push_str(&chars.next().unwrap().to_lowercase().to_string());

    for char in chars {
        if char.is_uppercase() {
            r.push_str("_");
            r.push(char);
        } else {
            r.push_str(&char.to_lowercase().to_string())
        }
    }

    r
}

#[proc_macro_derive(ChunkReaderReadArgs)]
pub fn derive_read_args(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse_macro_input!(input);

    if let syn::Data::Enum(data_enum) = ast.data {
        let functions = data_enum
            .variants
            .into_iter()
            .map(|variant| {
                let variant_ident = variant.ident;
                let fields_type = variant
                    .fields
                    .into_iter()
                    .map(|field| field.ty)
                    .collect::<Vec<_>>();

                let fields_bind = (0..fields_type.len())
                    .map(|i| format_ident!("x{}", i))
                    .collect::<Vec<_>>();
                let function_name =
                    format_ident!("{}", camel_to_lower_snake(&variant_ident.to_string()));

                quote! {
                    pub fn #function_name (self) -> ( #(#fields_type),* ) {
                        let _r = self;
                        #(
                            let (_r, #fields_bind) = _r.read();
                        )*
                        ( #(#fields_bind),* )
                    }
                }
            })
            .collect::<Vec<_>>();

        quote! {
            impl<'c> ChunkReader<'c, 1> {
                #(#functions)*
            }
        }
        .into()
    } else {
        panic!("only enum is supported")
    }
}

#[proc_macro_derive(ToOpCode)]
pub fn derive_to_op_code(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse_macro_input!(input);

    let data_ident = ast.ident;

    if let syn::Data::Enum(data_enum) = ast.data {
        let variants_name = data_enum
            .variants
            .into_iter()
            .map(|variant| variant.ident)
            .collect::<Vec<_>>();

        let op_codes_name = variants_name
            .iter()
            .map(|variant_name| {
                format_ident!("{}", camel_to_upper_snake(&variant_name.to_string()))
            })
            .collect::<Vec<_>>();

        quote! {
            impl #data_ident {
                pub fn to_op_code(&self) -> OpCode {
                    use #data_ident::*;
                    use OpCode::*;

                    match self {
                        #(
                            #variants_name(..) => #op_codes_name
                        ),*
                    }
                }
            }
        }
        .into()
    } else {
        panic!("only enum is supported")
    }
}
