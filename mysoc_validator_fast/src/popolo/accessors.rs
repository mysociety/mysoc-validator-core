//! Repeated Python field methods; invocations live beside each entity.
//!
//! For example, `str_getter!(PyMembership, with_membership, person_id)` would generate
//! a `#[getter] fn person_id(&self) -> PyResult<String>` which clones the field
//! inside `with_membership`. Optional fields use `opt_str_getter` instead. Setters
//! use `paste` to name `set_<field>` methods. These macros generate separate
//! `#[pymethods]` blocks so expansion happens before PyO3 processes each block;
//! the bridge therefore enables `multiple-pymethods`.
//!
//! A trailing `, ?` propagates a fallible accessor's error (organizations can be
//! removed); otherwise the accessor returns its closure's value directly. Stub
//! annotations describe Python results, so `PyResult<String>` remains `str`.
//! JSON conversion runs after access closures release the dataset lock.

/// Common ownership helpers; record lookup, redirects and deletion stay entity-specific.
macro_rules! standalone_handle_methods {
    ($struct:ty, $state:ident, $model:ty) => {
        impl $struct {
            pub fn make_handle(shared: super::SharedPopolo, id: String) -> Self {
                Self {
                    inner: $state::Handle { shared, id },
                }
            }

            pub fn get_standalone(&self) -> Option<&$model> {
                match &self.inner {
                    $state::Standalone(model) => Some(model),
                    $state::Handle { .. } => None,
                }
            }
        }
    };
}

/// Generate a `String` getter for `$field` using `$accessor`.
macro_rules! str_getter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self) -> PyResult<String> {
                Ok(self.$accessor(|inner| inner.$field.clone()) $($fallible)?)
            }
        }
    };
}

/// Generate an `Option<String>` getter for `$field` using `$accessor`.
macro_rules! opt_str_getter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self) -> PyResult<Option<String>> {
                Ok(self.$accessor(|inner| inner.$field.clone()) $($fallible)?)
            }
        }
    };
}

/// Generate a `String` setter for `$field` using `$accessor`.
macro_rules! str_setter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        ::paste::paste! {
            #[pyo3_stub_gen::derive::gen_stub_pymethods]
            #[pymethods]
            impl $struct {
                #[setter($field)]
                fn [<set_ $field>](&mut self, value: String) -> PyResult<()> {
                    self.$accessor(|inner| inner.$field = value) $($fallible)?;
                    Ok(())
                }
            }
        }
    };
}

/// Generate an `Option<String>` setter for `$field` using `$accessor`.
macro_rules! opt_str_setter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        ::paste::paste! {
            #[pyo3_stub_gen::derive::gen_stub_pymethods]
            #[pymethods]
            impl $struct {
                #[setter($field)]
                fn [<set_ $field>](&mut self, value: Option<String>) -> PyResult<()> {
                    self.$accessor(|inner| inner.$field = value) $($fallible)?;
                    Ok(())
                }
            }
        }
    };
}

/// Clone a date under the dataset lock, then create its Python value after release.
macro_rules! date_getter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self, py: Python<'_>) -> PyResult<Py<fuzzy_date::FuzzyDate>> {
                let date = self.$accessor(|inner| inner.$field.clone()) $($fallible)?;
                super::conversions::fuzzy_to_py(py, &date)
            }
        }
    };
}

/// Optional dates preserve None and share the fallible handle behavior of other getters.
macro_rules! opt_date_getter {
    ($struct:ty, $accessor:ident, $field:ident $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter]
            fn $field(&self, py: Python<'_>) -> PyResult<Option<Py<fuzzy_date::FuzzyDate>>> {
                let date = self.$accessor(|inner| inner.$field.clone()) $($fallible)?;
                date.as_ref().map(|date| super::conversions::fuzzy_to_py(py, date)).transpose()
            }
        }
    };
}

/// Convert Python input before acquiring a write lock; wrap optional domain fields.
macro_rules! converted_setter {
    ($struct:ty, $accessor:ident, $field:ident, $convert:path, $wrap:path $(, $fallible:tt)?) => {
        ::paste::paste! {
            #[pyo3_stub_gen::derive::gen_stub_pymethods]
            #[pymethods]
            impl $struct {
                #[setter($field)]
                fn [<set_ $field>](&mut self, value: &Bound<'_, PyAny>) -> PyResult<()> {
                    let value = $wrap($convert(value)?);
                    self.$accessor(|inner| inner.$field = value) $($fallible)?;
                    Ok(())
                }
            }
        }
    };
}

macro_rules! extra_methods {
    ($struct:ty, $accessor:ident, $mut_accessor:ident, [$($external:literal => $field:ident),+ $(,)?] $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[getter(extra)]
            fn extra_property(slf: PyRef<'_, Self>, py: Python<'_>) -> PyResult<Option<Py<PyExtra>>> {
                if slf.$accessor(|inner| inner.extra.is_some()) $($fallible)? {
                    Ok(Some(Py::new(py, PyExtra::new(slf.into_py(py)))?))
                } else {
                    Ok(None)
                }
            }

            fn _extra_dict(&self, py: Python<'_>) -> PyResult<PyObject> {
                let extra = self.$accessor(|inner| inner.extra.clone().unwrap_or_default()) $($fallible)?;
                json_value_to_py(py, &serde_json::Value::Object(extra))
            }

            fn get_extra(&self, py: Python<'_>, key: &str) -> PyResult<Option<PyObject>> {
                let value = self.$accessor(|inner| {
                    inner.extra.as_ref().and_then(|extra| extra.get(key)).cloned()
                }) $($fallible)?;
                value.as_ref().map(|value| json_value_to_py(py, value)).transpose()
            }

            fn set_extra<'py>(
                mut slf: PyRefMut<'py, Self>,
                key: String,
                value: &Bound<'py, PyAny>,
            ) -> PyResult<PyRefMut<'py, Self>> {
                let value = py_to_json_value(value)?;
                slf.$mut_accessor(|inner| {
                    inner.extra.get_or_insert_with(Default::default).insert(key, value);
                }) $($fallible)?;
                Ok(slf)
            }

            fn get_extra_as(
                &self,
                py: Python<'_>,
                model: &Bound<'_, PyAny>,
            ) -> PyResult<Option<PyObject>> {
                if !self.$accessor(|inner| inner.extra.is_some()) $($fallible)? {
                    return Ok(None);
                }
                let extra = self._extra_dict(py)?;
                Ok(Some(model.call_method1("model_validate", (extra,))?.into_py(py)))
            }

            fn get_localised_value(
                &self,
                field: &str,
                language: &str,
            ) -> PyResult<Option<String>> {
                if ![$($external),+].contains(&field) {
                    return Err(PyValueError::new_err(format!(
                        "Field {} not found on {}", field, stringify!($struct)
                    )));
                }
                self.$accessor(|inner| {
                    let localised = inner.extra.as_ref()
                        .and_then(|extra| extra.get("localised_values"))
                        .and_then(|value| value.get(field))
                        .and_then(|value| value.get(language))
                        .and_then(|value| value.as_str())
                        .map(str::to_owned);
                    Ok(localised.or_else(|| match field {
                        $( $external => inner.$field.to_optional_string(), )+
                        _ => None,
                    }))
                }) $($fallible)?
            }

            fn set_localised_value(
                &mut self,
                field: &str,
                language: &str,
                value: String,
            ) -> PyResult<()> {
                if ![$($external),+].contains(&field) {
                    return Err(PyValueError::new_err(format!(
                        "Field {} not found on {}", field, stringify!($struct)
                    )));
                }
                self.$mut_accessor(|inner| {
                    let extra = inner.extra.get_or_insert_with(Default::default);
                    let localised = extra.entry("localised_values").or_insert_with(|| {
                        serde_json::Value::Object(Default::default())
                    });
                    let fields = localised.as_object_mut().ok_or_else(|| PyValueError::new_err("localised_values must be an object"))?;
                    let languages = fields.entry(field).or_insert_with(|| {
                        serde_json::Value::Object(Default::default())
                    });
                    languages.as_object_mut().ok_or_else(|| PyValueError::new_err("localised field must be an object"))?
                        .insert(language.to_string(), serde_json::Value::String(value));
                    Ok(())
                }) $($fallible)?
            }
        }
    };
}

macro_rules! standalone_model_methods {
    ($struct:ty, $state:ident, $variant:ident, $rust_type:ty, $accessor:ident $(, $fallible:tt)?) => {
        #[pyo3_stub_gen::derive::gen_stub_pymethods]
        #[pymethods]
        impl $struct {
            #[staticmethod]
            fn model_validate(value: &Bound<'_, PyAny>) -> PyResult<Self> {
                let value = py_to_json_value(value)?;
                let model: $rust_type = serde_json::from_value(value)
                    .map_err(|error| PyValueError::new_err(error.to_string()))?;
                Ok(Self {
                    inner: $state::$variant(model),
                })
            }

            fn model_dump(&self, py: Python<'_>) -> PyResult<PyObject> {
                let value = self.$accessor(|model| serde_json::to_value(model)) $($fallible)?
                    .map_err(|error| PyValueError::new_err(error.to_string()))?;
                json_value_to_py(py, &value)
            }
        }
    };
}

pub(super) use {
    converted_setter, date_getter, extra_methods, opt_date_getter, opt_str_getter, opt_str_setter,
    standalone_handle_methods, standalone_model_methods, str_getter, str_setter,
};
