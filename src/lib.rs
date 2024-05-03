use proc_macro::TokenStream;
use syn::{DeriveInput, Data, Ident};
use double_dot_macro_types::*;

fn impl_double_states_trait(ast: DeriveInput) -> TokenStream {
    let enum_name = ast.ident;
    let (enum_name1, enum_name2) = (enum_name.to_string(), enum_name.to_string());

    match ast.data {
        Data::Enum(enum_data) => {
            let enum_str = enum_name.to_string();
            // all the enum fields
            let variants = enum_data.variants.clone();

            // a vec of idents with seperate Span's
            let mut ident_fields = Vec::new();
            for field in variants.iter() {
                let field_name = field.ident.to_string();
                ident_fields.push(make_ident(field_name.as_str()));
            }

            // a vec that holds strings for all the field names, linear, and arbitrary transition attributes
            let mut state_fields: Vec<StateField> = Vec::new();

            for field in variants.iter() {
                // the arbitrary transitions for the current enum field
                let mut arb_attr = String::new();
                // the linear transitions for the current enum field
                let mut lin_attr = String::new();
                // loop through the attributes for the current enum filed
                for attr in field.attrs.iter() {
                    // a vector of idents just to check if the next states specified as attrubutes exist as fields in the enum
                    let mut next_states: Vec<Ident> = Vec::new();
                    // num to check if we have found more than 1 linear transition as there shouldn't be more than 1
                    let mut i = 0;

                    // if the current attribute is linear
                    if attr.path().is_ident("linear") {
                        if let Err(_) = attr.parse_nested_meta(|meta| {
                            if i == 0 {
                                let ident = meta.path.get_ident().unwrap();
                                next_states.push(ident.clone());
                                lin_attr = ident.to_string();
                            }
                            i += 1;
                            Ok(())
                        }) {
                            // if there was a problem parsing the attributes parameteres
                            return syn::Error::new_spanned(
                                &field.ident,
                                format!("Problem parsing linear list for \"{}\"", field.ident.to_string()),
                            ).to_compile_error().into()
                        }
                        // if more than 1 linear transition was found give a compile error
                        if i > 1 {
                            return syn::Error::new_spanned(
                                &field.ident,
                                format!("Only 1 Linear Transition allowed. Error Occured On \"{}\"", field.ident.to_string()),
                            ).to_compile_error().into()
                        }
                    }
                    // if the current attribute is arbitrary
                    else if attr.path().is_ident("arbitrary") {
                        if let Err(_) = attr.parse_nested_meta(|meta| {
                            let ident = meta.path.get_ident().unwrap();
                            next_states.push(ident.clone());
                            arb_attr.push_str(format!("{},", ident.to_string()).as_str());
                            Ok(())
                        }) {
                            // if there was a problem parsing the attributes parameters
                            return syn::Error::new_spanned(
                                &field.ident,
                                format!("Problem parsing arbitrary list for \"{}\"", field.ident.to_string()),
                            ).to_compile_error().into()
                        }
                    }
                    // loop through the possible next_states found via attributes and check if they exist as fields in the enum
                    for next_state in next_states.iter() {
                        if let None = ident_fields.iter().find(|field| field.to_string() == next_state.to_string()) {
                            return syn::Error::new_spanned(
                                &next_state,
                                format!("\"{}\" doesn't exist as a state in \"{}\"", next_state.to_string(), enum_name.to_string()),
                            ).to_compile_error().into()
                        }
                    }
                }
                // push the current enum name, linear attributes, and arbitrary attributes as strings for later use in the methods
                state_fields.push(StateField::new(field.ident.to_string(), lin_attr, arb_attr))
            }

            // let idents = enum_data.variants.iter().map(|v| &v.ident);
            // let len = idents.len();
        
            // our DoubleStates implementations
            let double_state = quote::quote!{
                impl DoubleStates for #enum_name {
                    fn name(&self) -> &'static str {
                        #enum_str
                    }
                    fn to_string(&self) -> String {
                        match self {
                            #(Self::#ident_fields => {
                                return format!("{:?}", Self::#ident_fields)
                            })*
                        }
                    }
                    fn linear_transition(&self) -> Self {
                        // return value
                        let mut new_field = Self::default();
                        // the next linear state in the form of a string
                        let mut next_state = String::new();
                        // variants are compiled into an array only once to avoid multiple Idents with the same Span
                        let variants = [#(Self::#ident_fields,)*].into_iter();
                        // compile the state fields to an array for easy access
                        let state_fields = [#((#state_fields.0, #state_fields.1),)*].into_iter();

                        // used to check if a linear transition was found for self
                        let mut lin_found = false;
                        // find the linear transistion if there is one 
                        state_fields.clone().for_each(|f| {
                            // check if the field name string matched the self field name string
                            if f.0 == self.to_string() {
                                next_state = f.1.to_string();
                            }
                        });
                        // grab the value of the next state and set it to the return variable
                        variants.for_each(|var| {
                            if var.to_string() == next_state {
                                lin_found = true;
                                new_field = var.clone();
                            }
                        });

                        // if no linear transitions were found for self we should panic as this can be a program breaking bug
                        if !lin_found {
                            panic!("No linear transition found for \"{}::{:?}\"", #enum_name1, self);
                        }

                        return new_field
                    }
                    fn arbitrary_transition(&self, next_state: &Self) -> Self {
                        // return value to be set later
                        let mut arb_state = self.clone();
                        // variants are compiled into an array only once to avoid multiple Idents with the same Span
                        let mut variants = [#(Self::#ident_fields,)*].into_iter();
                        // compile the state fields to an array for easy access
                        let state_fields = [#((#state_fields.0, #state_fields.2),)*].into_iter();

                        // used to check if we found the specified arbitrary transition for self
                        let mut arb_found = false;
                        // grab the new arbitrary state and set it the return variable
                        // loop through each state field until we find the one that matches self
                        state_fields.for_each(|f| {
                            if f.0 == self.to_string() {
                                // once we find the proper state field parse the arbitrary transitions into a Vec and find the one that matches 
                                // the specified next_state passed in
                                self.parse_arbs(f.1).iter().for_each(|arb| {
                                    // if we found the arbirary state
                                    if arb.to_string() == next_state.to_string() {
                                        arb_found = true;
                                        arb_state = variants.find(|var| var == next_state).unwrap();
                                    }
                                });
                            }
                        });
                        // if we didn't find the arbirary state we should panic as this can be a program breaking bug
                        if !arb_found {
                            panic!("Arbitrary transition \"{:?}\" not found for \"{}::{:?}\"", next_state, #enum_name2, self);
                        }
                        return arb_state
                    }
                    fn parse_arbs(&self, arbs: &str) -> Vec<String> {
                        arbs.split(',').map(|s| s.trim().to_string()).collect()
                    }
                }
            };
        
            // impl bevy's States so the user doesn't have to 
            let bevy_states = quote::quote! {
                impl bevy::prelude::States for #enum_name {
                    // // Bevy's States trait requires specific methods to be implemented
                    // // Implement the required methods here
                    // type Iter = std::array::IntoIter<Self, #len>;
        
                    // fn variants() -> Self::Iter {
                    //     [#(Self::#ident_fields,)*].into_iter()
                    // }
                }
            };
            
            // compile the bevy States impl with the DoubleStates impl 
            quote::quote!{
                #double_state
                #bevy_states
            }.into()
        },
        _ => {
            syn::Error::new_spanned(
                &enum_name,
                "DoubleStates can only be derived for enums with variants",
            )
            .to_compile_error()
            .into()
        }
    }

}
 
#[proc_macro_derive(DoubleStates, attributes(linear, arbitrary))]
pub fn double_states_derive_macro(item: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(item).unwrap();

    impl_double_states_trait(ast)
}