//! Compiler/runtime operations used by async lowering and std::task.
use crate::ast::WaveType;

pub fn is_intrinsic(name: &str) -> bool {
    matches!(
        name,
        "__wave_async_alloc"
            | "__wave_async_create"
            | "__wave_async_complete"
            | "__wave_async_wait"
            | "__wave_async_ready"
            | "__wave_async_take"
            | "__wave_async_block_on"
            | "__wave_async_spawn"
            | "__wave_async_cancel"
            | "__wave_async_yield"
            | "__wave_async_shutdown"
            | "__wave_async_invoke"
            | "__wave_async_free_slot"
            | "__wave_async_interest"
            | "__wave_async_sleep"
            | "__wave_async_close_fd"
            | "__wave_async_cancel_join"
            | "__wave_async_io"
            | "__wave_async_windows_notify_address"
    )
}
/// The caller validates argument expressions before checking this signature.
pub fn signature(
    name: &str,
    types: &[WaveType],
    arguments: &[WaveType],
) -> Result<(Vec<WaveType>, WaveType), String> {
    use WaveType::*;
    let ptr = |t| Pointer(Box::new(t));
    let future = |t| Future(Box::new(t));
    let scalar = || {
        arguments
            .last()
            .and_then(|t| if let Future(t) = t { Some(*t.clone()) } else { None })
            .ok_or_else(|| format!("{name} requires a Future<T>"))
    };
    let no_types = || {
        if types.is_empty() {
            Ok(())
        } else {
            Err(format!("{name} does not accept explicit type arguments"))
        }
    };
    Ok(match name {
        "__wave_async_alloc" if types.len() == 1 => (vec![], ptr(types[0].clone())),
        "__wave_async_create" if types.len() == 2 => (
            vec![
                ptr(types[0].clone()),
                ptr(if types[1] == Void { Byte } else { types[1].clone() }),
                String,
            ],
            future(types[1].clone()),
        ),
        "__wave_async_complete" => {
            no_types()?;
            (vec![Int(64)], Void)
        },
        "__wave_async_wait" => {
            no_types()?;
            let t = scalar()?;
            (vec![Int(64), future(t)], Void)
        },
        "__wave_async_ready" | "__wave_async_cancel" => {
            no_types()?;
            let t = scalar()?;
            (vec![future(t)], Bool)
        },
        "__wave_async_take" | "__wave_async_block_on" => {
            no_types()?;
            let t = scalar()?;
            (vec![future(t.clone())], t)
        },
        "__wave_async_spawn" => {
            no_types()?;
            let t = scalar()?;
            (vec![future(t.clone())], future(t))
        },
        "__wave_async_yield" => {
            no_types()?;
            (vec![], future(Void))
        },
        "__wave_async_shutdown" => {
            no_types()?;
            (vec![], Void)
        },
        "__wave_async_free_slot" if arguments.len() == 1 && matches!(&arguments[0], Pointer(_)) => {
            no_types()?;
            (arguments.to_vec(), Void)
        },
        "__wave_async_interest" => {
            no_types()?;
            (vec![Int(64), Int(32), Int(64)], future(Int(32)))
        },
        "__wave_async_sleep" => {
            no_types()?;
            (vec![Int(64)], future(Int(32)))
        },
        "__wave_async_cancel_join" => {
            no_types()?;
            let t = scalar()?;
            (vec![future(t)], future(Void))
        },
        "__wave_async_io" => {
            no_types()?;
            (vec![Int(64), ptr(Uint(8)), Int(64), Int(32), Int(64)], future(Int(64)))
        },
        "__wave_async_windows_notify_address" => {
            no_types()?;
            (vec![], ptr(Uint(8)))
        },
        "__wave_async_close_fd" => {
            no_types()?;
            (vec![Int(64)], Void)
        },
        "__wave_async_invoke" => {
            no_types()?;
            (vec![ptr(Uint(8)), ptr(Uint(8)), Int(64)], Bool)
        },
        _ => return Err(format!("invalid async intrinsic {name}")),
    })
}

/// Runtime entry points needed by an intrinsic after async frame lowering.
pub fn runtime_symbols(name: &str) -> &'static [&'static str] {
    match name {
        "__wave_async_alloc" => &["__wave_task_alloc"],
        "__wave_async_free_slot" => &["__wave_task_free"],
        "__wave_async_create" => &["__wave_task_new"],
        "__wave_async_take" => &["__wave_task_result", "__wave_task_release"],
        "__wave_async_block_on" => {
            &["__wave_task_drive", "__wave_task_result", "__wave_task_release"]
        },
        "__wave_async_ready" => &["__wave_task_ready"],
        "__wave_async_wait" => &["__wave_task_wait"],
        "__wave_async_complete" => &["__wave_task_complete"],
        "__wave_async_spawn" => &["__wave_task_spawn"],
        "__wave_async_cancel" => &["__wave_task_cancel"],
        "__wave_async_cancel_join" => &["__wave_task_cancel_join"],
        "__wave_async_yield" => &["__wave_task_yield"],
        "__wave_async_shutdown" => &["__wave_task_shutdown"],
        "__wave_async_interest" | "__wave_async_sleep" => &["__wave_task_interest"],
        "__wave_async_close_fd" => &["__wave_task_close_fd"],
        "__wave_async_io" => &["__wave_task_io"],
        "__wave_async_windows_notify_address" => &["__wave_task_windows_notify"],
        _ => &[],
    }
}

/// Scalar and pointer boundary shared by generated frames and the Wave executor.
pub fn runtime_signature(symbol: &str) -> Option<(Vec<WaveType>, WaveType, &'static str)> {
    use WaveType::*;
    let pointer = || Pointer(Box::new(Uint(8)));
    let (args, result) = match symbol {
        "__wave_task_alloc" => (vec![Int(64)], pointer()),
        "__wave_task_free" => (vec![pointer(), Int(64)], Void),
        "__wave_task_new" => (vec![pointer(), Int(64), pointer(), pointer()], Int(64)),
        "__wave_task_result" => (vec![Int(64)], pointer()),
        "__wave_task_release"
        | "__wave_task_drive"
        | "__wave_task_complete"
        | "__wave_task_close_fd" => (vec![Int(64)], Void),
        "__wave_task_ready" | "__wave_task_cancel" => (vec![Int(64)], Int(32)),
        "__wave_task_spawn" | "__wave_task_cancel_join" => (vec![Int(64)], Int(64)),
        "__wave_task_wait" => (vec![Int(64), Int(64)], Void),
        "__wave_task_yield" => (vec![], Int(64)),
        "__wave_task_shutdown" => (vec![], Void),
        "__wave_task_interest" => (vec![Int(64), Int(32), Int(64)], Int(64)),
        "__wave_task_io" => (vec![Int(64), pointer(), Int(64), Int(32), Int(64)], Int(64)),
        "__wave_task_windows_notify" => return Some((vec![pointer(), Uint(8)], Void, "system")),
        _ => return None,
    };
    Some((args, result, "c"))
}
