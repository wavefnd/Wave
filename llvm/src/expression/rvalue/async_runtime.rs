//! Private task runtime calls. Frame allocation sizes use the selected LLVM layout.
use super::ExprGenEnv;
use crate::codegen::types::{wave_type_to_llvm_type, TypeFlavor};
use inkwell::{
    values::{BasicMetadataValueEnum, BasicValueEnum},
    IntPredicate,
};
use parser::ast::{Expression, Literal, WaveType};
fn runtime<'ctx>(
    env: &ExprGenEnv<'ctx, '_>,
    name: &str,
    args: &[BasicMetadataValueEnum<'ctx>],
) -> BasicValueEnum<'ctx> {
    let function = env.module.get_function(name).expect("async runtime validated before emission");
    env.builder
        .build_call(function, args, "task")
        .unwrap()
        .try_as_basic_value()
        .basic()
        .unwrap_or_else(|| env.context.i8_type().const_zero().into())
}

pub(crate) fn gen<'ctx>(
    env: &mut ExprGenEnv<'ctx, '_>,
    name: &str,
    types: &[WaveType],
    args: &[Expression],
) -> BasicValueEnum<'ctx> {
    let i64t = env.context.i64_type();
    match name {
        "__wave_async_alloc" => {
            let ty =
                wave_type_to_llvm_type(env.context, &types[0], env.struct_types, TypeFlavor::Value);
            runtime(
                env,
                "__wave_task_alloc",
                &[i64t.const_int(env.target_data.get_abi_size(&ty), false).into()],
            )
        },
        "__wave_async_create" => {
            let frame = env.gen(&args[0], None);
            let result = env.gen(&args[1], None);
            let Expression::Literal(Literal::String(name)) = args[2].unspanned() else {
                unreachable!("generated poll symbol")
            };
            let poll = env
                .module
                .get_function(std::str::from_utf8(name).expect("validated generated resume symbol"))
                .expect("declared async resume function")
                .as_global_value()
                .as_pointer_value();
            let ty =
                wave_type_to_llvm_type(env.context, &types[0], env.struct_types, TypeFlavor::Value);
            runtime(
                env,
                "__wave_task_new",
                &[
                    frame.into(),
                    i64t.const_int(env.target_data.get_abi_size(&ty), false).into(),
                    result.into(),
                    poll.into(),
                ],
            )
        },
        "__wave_async_take" | "__wave_async_block_on" => {
            let Some(WaveType::Future(result)) = env.wave_type(&args[0]) else {
                unreachable!("typed future operand")
            };
            let id = env.gen(&args[0], Some(i64t.into()));
            if name == "__wave_async_block_on" {
                runtime(env, "__wave_task_drive", &[id.into()]);
            }
            let pointer = runtime(env, "__wave_task_result", &[id.into()]).into_pointer_value();
            let result = if *result == WaveType::Void {
                env.context.i8_type().const_zero().into()
            } else {
                let ty = wave_type_to_llvm_type(
                    env.context,
                    &result,
                    env.struct_types,
                    TypeFlavor::Value,
                );
                env.builder.build_load(ty, pointer, "task_result").unwrap()
            };
            runtime(env, "__wave_task_release", &[id.into()]);
            result
        },
        "__wave_async_interest" | "__wave_async_sleep" => {
            let values = if name == "__wave_async_sleep" {
                let ms = env.gen(&args[0], Some(i64t.into()));
                vec![
                    i64t.const_all_ones().into(),
                    env.context.i32_type().const_zero().into(),
                    ms.into(),
                ]
            } else {
                let fd = env.gen(&args[0], Some(i64t.into()));
                let flags = env.gen(&args[1], Some(env.context.i32_type().into()));
                let ms = env.gen(&args[2], Some(i64t.into()));
                vec![fd.into(), flags.into(), ms.into()]
            };
            runtime(env, "__wave_task_interest", &values)
        },
        "__wave_async_windows_notify_address" => env
            .module
            .get_function("__wave_task_windows_notify")
            .expect("Windows wait callback declared by std::task")
            .as_global_value()
            .as_pointer_value()
            .into(),
        "__wave_async_io" => {
            let fd = env.gen(&args[0], Some(i64t.into()));
            let buffer = env.gen(&args[1], None);
            let length = env.gen(&args[2], Some(i64t.into()));
            let writing = env.gen(&args[3], Some(env.context.i32_type().into()));
            let timeout = env.gen(&args[4], Some(i64t.into()));
            runtime(
                env,
                "__wave_task_io",
                &[fd.into(), buffer.into(), length.into(), writing.into(), timeout.into()],
            )
        },
        "__wave_async_invoke" => {
            let callback = env.gen(&args[0], None).into_pointer_value();
            let frame = env.gen(&args[1], None);
            let id = env.gen(&args[2], Some(i64t.into()));
            let ty = env
                .context
                .bool_type()
                .fn_type(&[env.context.ptr_type(Default::default()).into(), i64t.into()], false);
            env.builder
                .build_indirect_call(ty, callback, &[frame.into(), id.into()], "resume")
                .unwrap()
                .try_as_basic_value()
                .basic()
                .unwrap()
        },
        "__wave_async_free_slot" => {
            let Some(WaveType::Pointer(t)) = env.wave_type(&args[0]) else { unreachable!() };
            let pointer = env.gen(&args[0], None);
            let ty = wave_type_to_llvm_type(env.context, &t, env.struct_types, TypeFlavor::Value);
            runtime(
                env,
                "__wave_task_free",
                &[pointer.into(), i64t.const_int(env.target_data.get_abi_size(&ty), false).into()],
            )
        },
        _ => {
            let target = match name {
                "__wave_async_ready" => "__wave_task_ready",
                "__wave_async_wait" => "__wave_task_wait",
                "__wave_async_complete" => "__wave_task_complete",
                "__wave_async_spawn" => "__wave_task_spawn",
                "__wave_async_cancel" => "__wave_task_cancel",
                "__wave_async_cancel_join" => "__wave_task_cancel_join",
                "__wave_async_yield" => "__wave_task_yield",
                "__wave_async_shutdown" => "__wave_task_shutdown",
                "__wave_async_close_fd" => "__wave_task_close_fd",
                _ => unreachable!("known async intrinsic"),
            };
            let values =
                args.iter().map(|a| env.gen(a, Some(i64t.into())).into()).collect::<Vec<_>>();
            let result = runtime(env, target, &values);
            if matches!(name, "__wave_async_ready" | "__wave_async_cancel") {
                env.builder
                    .build_int_compare(
                        IntPredicate::NE,
                        result.into_int_value(),
                        env.context.i32_type().const_zero(),
                        "task_flag",
                    )
                    .unwrap()
                    .into()
            } else {
                result
            }
        },
    }
}
