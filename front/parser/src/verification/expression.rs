// SPDX-License-Identifier: MPL-2.0
//! Expression checks keep per-variant temporaries out of unrelated recursive
//! frames. The 128-level frontend limit also applies to debug compiler builds.
use super::*;

impl Validator<'_> {
    pub(super) fn validate_expr_inner(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        match expression {
            Expression::Located { .. } => unreachable!("source wrappers detached before analysis"),
            Expression::Literal(..) => self.validate_expr_literal(expression, expected),
            Expression::Null => Ok(ExpressionType::Null),
            Expression::Variable(..) => self.validate_expr_variable(expression, expected),
            Expression::Grouped(inner) => self.validate_expr_expected(inner, expected),
            Expression::Cast { .. } => self.validate_expr_cast(expression, expected),
            Expression::AddressOf(..) => self.validate_expr_address_of(expression, expected),
            Expression::Deref(..) => self.validate_expr_deref(expression, expected),
            Expression::BinaryExpression { .. } => {
                self.validate_expr_binary_expression(expression, expected)
            },
            Expression::Await(..) => self.validate_expr_await(expression, expected),
            Expression::Unary { .. } => self.validate_expr_unary(expression, expected),
            Expression::FunctionCall { .. } => {
                self.validate_expr_function_call(expression, expected)
            },
            Expression::MethodCall { .. } => self.validate_expr_method_call(expression, expected),
            Expression::StructLiteral { .. } => {
                self.validate_expr_struct_literal(expression, expected)
            },
            Expression::FieldAccess { .. } => self.validate_expr_field_access(expression, expected),
            Expression::IndexAccess { .. } => self.validate_expr_index_access(expression, expected),
            Expression::ArrayLiteral(..) => self.validate_expr_array_literal(expression, expected),
            Expression::Assignment { .. } => self.validate_expr_assignment(expression, expected),
            Expression::AssignOperation { .. } => {
                self.validate_expr_assign_operation(expression, expected)
            },
            Expression::IncDec { .. } => self.validate_expr_inc_dec(expression, expected),
            Expression::AsmBlock { .. } => self.validate_expr_asm_block(expression, expected),
        }
    }

    fn validate_expr_literal(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Literal(literal) = expression else { unreachable!("expression dispatch") };
        Ok(match literal {
            Literal::Int(raw) => {
                if let Some(WaveType::Float(bits)) =
                    expected.map(|ty| self.program.canonical_type(ty))
                {
                    let value = lexer::number::IntegerLiteral::parse(raw)
                        .and_then(|value| value.to_f64())
                        .ok_or_else(|| {
                            "integer literal is out of range for floating-point conversion"
                                .to_string()
                        })?;
                    if bits == 32 && !(value as f32).is_finite() {
                        return Err(
                            "integer literal is out of range for f32 conversion".to_string()
                        );
                    }
                }
                ExpressionType::IntLiteral(raw.clone())
            },
            Literal::Float(value) => {
                if !value.is_finite()
                    || (matches!(
                        expected.map(|ty| self.program.canonical_type(ty)),
                        Some(WaveType::Float(32))
                    ) && !(*value as f32).is_finite())
                {
                    return Err(
                        "floating-point literal is out of range for its expected type".to_string()
                    );
                }
                ExpressionType::FloatLiteral
            },
            Literal::String(_) => ExpressionType::Known(WaveType::String),
            Literal::Bool(_) => ExpressionType::Known(WaveType::Bool),
            Literal::Char(_) => ExpressionType::Known(WaveType::Char),
            Literal::Byte(_) => ExpressionType::Known(WaveType::Byte),
        })
    }

    fn validate_expr_variable(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Variable(name) = expression else { unreachable!("expression dispatch") };

        if self.program.variant_constructor(name).is_some() {
            return self.validate_variant_constructor(expression, name, &[], expected);
        }
        if let Some(binding) = self.lookup_binding(name) {
            Ok(ExpressionType::Known(binding.ty))
        } else {
            self.mark_span(SemanticSpanKind::Identifier, name.clone());
            Err(format!("use of undeclared identifier `{}`", name))
        }
    }

    fn validate_expr_cast(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Cast { expr, target_type } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, "as");
        self.program.validate_type(target_type, &self.current_type_params, false, "cast target")?;
        let source = self.validate_expr_expected(expr, Some(target_type))?;
        if !self.is_valid_cast(&source, target_type) {
            return Err(format!(
                "invalid cast from `{}` to `{}`",
                display_expression_type(&source),
                display_wave_type(target_type)
            ));
        }
        Ok(ExpressionType::Known(target_type.clone()))
    }

    fn validate_expr_address_of(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::AddressOf(inner) = expression else { unreachable!("expression dispatch") };

        self.mark_span(SemanticSpanKind::Keyword, "&");
        if !is_lvalue_expression(inner) && !matches!(inner.as_ref(), Expression::ArrayLiteral(_)) {
            return Err("cannot take the address of a non-lvalue expression".to_string());
        }
        if !matches!(inner.as_ref(), Expression::ArrayLiteral(_)) {
            self.ensure_mutable_write_target(inner, "take the address of")?;
        }
        // Addressed literals borrow their complete array layout from
        // the validated pointer destination, including element widths.
        let array_context =
            expected.map(|ty| self.program.canonical_type(ty)).and_then(|ty| match ty {
                WaveType::Pointer(pointee)
                    if matches!(inner.as_ref(), Expression::ArrayLiteral(_))
                        && matches!(pointee.as_ref(), WaveType::Array(_, _)) =>
                {
                    Some(*pointee)
                },
                _ => None,
            });
        let inner_type = self.validate_expr_expected(inner, array_context.as_ref())?;
        let inner_type = match inner_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        Ok(match inner_type {
            ExpressionType::Known(ty) => ExpressionType::Known(WaveType::Pointer(Box::new(ty))),
            ExpressionType::ArrayLiteral(elements) => {
                ExpressionType::AddressedArrayLiteral(elements)
            },
            _ => ExpressionType::Unknown,
        })
    }

    fn validate_expr_deref(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Deref(inner) = expression else { unreachable!("expression dispatch") };

        self.mark_span(SemanticSpanKind::Keyword, "deref");
        let mut projection = inner.as_ref();
        while let Expression::Grouped(nested) = projection.unspanned() {
            projection = nested;
        }
        if matches!(
            projection.unspanned(),
            Expression::FieldAccess { .. } | Expression::IndexAccess { .. }
        ) {
            return self.validate_expr(inner);
        }
        let inner_type = self.validate_expr(inner)?;
        let inner_type = match inner_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        match inner_type {
            ExpressionType::Known(WaveType::Pointer(ty)) => Ok(ExpressionType::Known(*ty)),
            other => {
                Err(format!("deref expects a pointer, found `{}`", display_expression_type(&other)))
            },
        }
    }

    fn validate_expr_binary_expression(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::BinaryExpression { left, operator, right } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(
            SemanticSpanKind::Keyword,
            operator_source_symbol(operator).unwrap_or("binary operator"),
        );
        if matches!(operator, Operator::ShiftLeft | Operator::ShiftRight) {
            let left_type = self.validate_expr_expected(left, expected)?;
            let right_type = self.validate_expr(right)?;
            return infer_binary_type(self.program, operator, left_type, right_type);
        }
        let left_contextual = left.is_contextual_integer();
        let right_contextual = right.is_contextual_integer();
        let (left_type, right_type) = match (left_contextual, right_contextual) {
            (true, true) => {
                // Only an arithmetic result may borrow its destination
                // width. A comparison's destination is boolean.
                let operand_type = expected
                    .filter(|_| expression.is_contextual_integer())
                    .map(|ty| self.program.canonical_type(ty))
                    .filter(|ty| matches!(ty, WaveType::Int(_) | WaveType::Uint(_)))
                    .unwrap_or(WaveType::Int(32));
                (
                    self.validate_integer_operand(left, &operand_type)?,
                    self.validate_integer_operand(right, &operand_type)?,
                )
            },
            (true, false) => {
                let right_type = self.validate_expr(right)?;
                let left_type = self.validate_contextual_operand(left, &right_type)?;
                (left_type, right_type)
            },
            (false, true) => {
                let left_type = self.validate_expr(left)?;
                let right_type = self.validate_contextual_operand(right, &left_type)?;
                (left_type, right_type)
            },
            (false, false) => (self.validate_expr(left)?, self.validate_expr(right)?),
        };
        infer_binary_type(self.program, operator, left_type, right_type)
    }

    fn validate_expr_await(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Await(inner) = expression else { unreachable!("expression dispatch") };

        if !self.current_async {
            return Err("await is only valid inside an async function".into());
        }
        match self.validate_expr(inner)? {
            ExpressionType::Known(WaveType::Future(result)) => Ok(ExpressionType::Known(*result)),
            other => Err(format!(
                "await requires a Future<T>, found `{}`",
                display_expression_type(&other)
            )),
        }
    }

    fn validate_expr_unary(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Unary { operator, expr } = expression else {
            unreachable!("expression dispatch")
        };

        let operand_expected = expected.filter(|_| expression.is_contextual_integer());
        let ty = self.validate_expr_expected(expr, operand_expected)?;
        self.validate_unary(operator, ty)
    }

    fn validate_expr_function_call(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::FunctionCall { name, type_args, args } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Identifier, name.clone());
        if self.program.variant_constructor(name).is_some() {
            if !type_args.is_empty() {
                return Err(format!(
                    "variant constructor `{}` does not accept function type arguments",
                    name
                ));
            }
            self.validate_variant_constructor(expression, name, args, expected)
        } else {
            self.validate_function_call(expression, name, type_args, args)
        }
    }

    fn validate_expr_method_call(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::MethodCall { object, name, args, type_args } = expression else {
            unreachable!("expression dispatch")
        };
        self.validate_method_call(expression, object, name, type_args, args)
    }

    fn validate_expr_struct_literal(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::StructLiteral { name, fields } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Identifier, name.clone());
        let known_fields = self
            .program
            .struct_fields(name)
            .cloned()
            .ok_or_else(|| format!("unknown struct `{}`", name))?;
        let mut provided = HashSet::new();
        for (field_name, value) in fields {
            self.mark_span(SemanticSpanKind::Declaration, field_name.clone());
            if !provided.insert(field_name.as_str()) {
                return Err(format!(
                    "struct literal `{}` initializes field `{}` more than once",
                    name, field_name
                ));
            }
            let expected = self
                .program
                .struct_field_type(name, field_name)
                .ok_or_else(|| format!("struct `{}` has no field `{}`", name, field_name))?;
            let actual = self.validate_expr_expected(value, Some(&expected))?;
            self.require_assignable(
                &actual,
                &expected,
                &format!("field `{}.{}`", name, field_name),
            )?;
        }
        let mut missing: Vec<&str> = known_fields
            .keys()
            .map(String::as_str)
            .filter(|field| !provided.contains(field))
            .collect();
        missing.sort_unstable();
        if !missing.is_empty() {
            return Err(format!(
                "struct literal `{}` is missing field(s): {}",
                name,
                missing.join(", ")
            ));
        }
        Ok(ExpressionType::Known(WaveType::Struct(name.clone())))
    }

    fn validate_expr_field_access(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::FieldAccess { object, field } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Identifier, field.clone());
        let object_type = self.validate_expr(object)?;
        let object_type = match object_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        if let Some(span) = self.source_map.expressions.get(&(expression as *const _ as usize)) {
            self.source_span = Some(span.clone());
        }
        let structure = match &object_type {
            ExpressionType::Known(WaveType::Struct(name)) => Some(name.clone()),
            ExpressionType::Known(WaveType::Pointer(inner)) => match inner.as_ref() {
                WaveType::Struct(name) => Some(name.clone()),
                _ => None,
            },
            _ => None,
        };

        let structure = structure.ok_or_else(|| {
            format!(
                "field access requires a struct or pointer-to-struct, found `{}`",
                display_expression_type(&object_type)
            )
        })?;
        let field_type = self
            .program
            .struct_field_type(&structure, field)
            .ok_or_else(|| format!("struct `{}` has no field `{}`", structure, field))?;
        Ok(ExpressionType::Known(field_type))
    }

    fn validate_expr_index_access(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::IndexAccess { target, index } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, "[");
        let target_type = self.validate_expr(target)?;
        let target_type = match target_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        let index_type = self.validate_expr(index)?;
        if !self.is_integer_expression(&index_type) {
            return Err(format!(
                "index expression must be an integer, found `{}`",
                display_expression_type(&index_type)
            ));
        }
        match target_type {
            ExpressionType::Known(WaveType::String) => Ok(ExpressionType::Known(WaveType::Int(8))),
            ExpressionType::Known(WaveType::Array(element, _)) => {
                Ok(ExpressionType::Known(*element))
            },
            ExpressionType::Known(WaveType::Pointer(element)) => match *element {
                WaveType::Array(array_element, _) => Ok(ExpressionType::Known(*array_element)),
                other => Ok(ExpressionType::Known(other)),
            },
            other => Err(format!(
                "index access requires an array or pointer, found `{}`",
                display_expression_type(&other)
            )),
        }
    }

    fn validate_expr_array_literal(
        &mut self,
        expression: &Expression,
        expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::ArrayLiteral(values) = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, "[");
        let expected_element =
            expected.map(|ty| self.program.canonical_type(ty)).and_then(|ty| match ty {
                WaveType::Array(element, _) => Some(*element),
                _ => None,
            });
        let mut element_types = Vec::with_capacity(values.len());
        for value in values {
            element_types.push(self.validate_expr_expected(value, expected_element.as_ref())?);
        }
        Ok(ExpressionType::ArrayLiteral(element_types))
    }

    fn validate_expr_assignment(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::Assignment { target, value } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, "=");
        if !is_lvalue_expression(target) {
            return Err("assignment target is not an lvalue".to_string());
        }
        self.ensure_mutable_write_target(target, "assign")?;
        let target_type = self.validate_expr(target)?;
        let target_type = match target_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        let value_type = if let ExpressionType::Known(expected) = &target_type {
            self.validate_expr_expected(value, Some(expected))?
        } else {
            self.validate_expr(value)?
        };
        if let ExpressionType::Known(expected) = &target_type {
            let context = find_base_var(target, false)
                .map(|(name, _)| format!("assignment to `{}`", name))
                .unwrap_or_else(|| "assignment expression".to_string());
            self.require_assignable(&value_type, expected, &context)?;
        }
        Ok(target_type)
    }

    fn validate_expr_assign_operation(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::AssignOperation { target, operator, value } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, assign_operator_source_symbol(operator));
        if !is_lvalue_expression(target) {
            return Err("compound assignment target is not an lvalue".to_string());
        }
        self.ensure_mutable_write_target(target, "modify with compound assignment")?;
        let target_type = self.validate_expr(target)?;
        let target_type = match target_type {
            ExpressionType::Known(ty) => ExpressionType::Known(self.program.canonical_type(&ty)),
            other => other,
        };
        let value_type = if let ExpressionType::Known(expected) = &target_type {
            self.validate_expr_expected(value, Some(expected))?
        } else {
            self.validate_expr(value)?
        };
        if matches!(operator, AssignOperator::Assign) {
            if let ExpressionType::Known(expected) = &target_type {
                let context = find_base_var(target, false)
                    .map(|(name, _)| format!("assignment to `{}`", name))
                    .unwrap_or_else(|| "assignment expression".to_string());
                self.require_assignable(&value_type, expected, &context)?;
            }
            return Ok(target_type);
        }
        let target_is_numeric = match &target_type {
            ExpressionType::Known(ty) => is_numeric_type(&self.program.canonical_type(ty)),
            _ => false,
        };
        let value_is_numeric = match &value_type {
            ExpressionType::IntLiteral(_) | ExpressionType::FloatLiteral => true,
            ExpressionType::Known(ty) => is_numeric_type(&self.program.canonical_type(ty)),
            _ => false,
        };
        if !target_is_numeric || !value_is_numeric {
            return Err(format!(
                "compound assignment `{:?}` requires numeric operands, found `{}` and `{}`",
                operator,
                display_expression_type(&target_type),
                display_expression_type(&value_type)
            ));
        }
        if let ExpressionType::Known(expected) = &target_type {
            self.require_assignable(&value_type, expected, "right operand of compound assignment")?;
        }
        Ok(target_type)
    }

    fn validate_expr_inc_dec(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::IncDec { target, .. } = expression else {
            unreachable!("expression dispatch")
        };

        self.mark_span(SemanticSpanKind::Keyword, "++|--");
        if !is_lvalue_expression(target) {
            return Err("++/-- target is not an lvalue".to_string());
        }
        self.ensure_mutable_write_target(target, "modify with ++/--")?;
        let ty = self.validate_expr(target)?;
        let supported = match &ty {
            ExpressionType::Known(ty) => matches!(
                self.program.canonical_type(ty),
                WaveType::Int(_)
                    | WaveType::Uint(_)
                    | WaveType::Float(_)
                    | WaveType::Char
                    | WaveType::Byte
                    | WaveType::Pointer(_)
            ),
            _ => false,
        };
        if !supported {
            return Err(format!(
                "++/-- requires a numeric or pointer lvalue, found `{}`",
                display_expression_type(&ty)
            ));
        }
        Ok(ty)
    }

    fn validate_expr_asm_block(
        &mut self,
        expression: &Expression,
        _expected: Option<&WaveType>,
    ) -> Result<ExpressionType, String> {
        let Expression::AsmBlock { inputs, outputs, .. } = expression else {
            unreachable!("expression dispatch")
        };

        for (_, expression) in inputs.iter().chain(outputs.iter()) {
            self.validate_expr(expression)?;
        }
        Ok(ExpressionType::Unknown)
    }
}
