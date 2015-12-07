from . import bytecode, compilercontext, objectspace


def load_var(ctx, name):
    if ctx.is_local(name):
        op = bytecode.LOAD_VAR
    else:
        op = bytecode.LOAD_GLOBAL
    vnum = ctx.register_var(name)
    ctx.emit(op, vnum)


def load_global(ctx, name):
    vnum = ctx.register_var(name)
    ctx.emit(bytecode.LOAD_GLOBAL, vnum)


def load_attr(ctx, name):
    # FIXME: Should probably use a different list than the vars,
    # so as not to allocate storage space for these names
    ctx.emit(bytecode.LOAD_ATTR, ctx.register_var(name))


def set_attr(ctx, name):
    # FIXME: Should probably use a different list than the vars,
    # so as not to allocate storage space for these names
    ctx.emit(bytecode.SET_ATTR, ctx.register_var(name))


def load_constant_int(ctx, val):
    vnum = ctx.register_constant(objectspace.W_Int(val))
    ctx.emit(bytecode.LOAD_CONSTANT, vnum)
    return vnum


def load_constant_string(ctx, val):
    ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(objectspace.W_String(val)))


def make_function(ctx, name, code_cb, args):
    cctx = compilercontext.CompilerContext()
    for arg in args:
        cctx.register_var(arg)
    cctx.locals = args
    code_cb(cctx)
    cctx.emit(bytecode.LOAD_CONSTANT, cctx.register_constant(objectspace.TheNone))
    cctx.emit(bytecode.RETURN)
    # TODO: record number of args on the code object
    code = cctx.create_bytecode()
    code_const = ctx.register_constant(code)
    ctx.emit(bytecode.LOAD_CONSTANT, code_const)
    ctx.emit(bytecode.MAKE_FUNCTION)
    fvar = ctx.register_var(name)
    ctx.emit(bytecode.ASSIGN, fvar)
    return fvar


def binary_operation(ctx, op_str):
    ctx.emit(bytecode.BINOP[op_str])


def assignment(ctx, name):
    vnum = ctx.register_var(name)
    ctx.emit(bytecode.ASSIGN, vnum)
    return vnum


def do_print(ctx):
    # XXX: print with multiple args?
    ctx.emit(bytecode.PRINT)


def function_call(ctx, numargs, args_cb):
    args_cb(ctx)
    ctx.emit(bytecode.CALL_FUNCTION, numargs)


def conditional(ctx, true_block_cb, false_block_cb):
    # TODO: can optimise out the jump after the true block if there is no false block
    jump_back_instr = ctx.next_instruction_index()
    ctx.emit(bytecode.JUMP_IF_FALSE)
    true_block_cb(ctx)
    jump_forward_instr = ctx.next_instruction_index()
    ctx.emit(bytecode.JUMP_FORWARD)
    ctx.adjust_arg(jump_back_instr, ctx.next_instruction_index()-jump_back_instr-2)
    false_block_cb(ctx)
    ctx.adjust_arg(jump_forward_instr, ctx.next_instruction_index()-jump_forward_instr-2)


def while_loop(ctx, condition_cb, block_cb):
    start_instr = ctx.next_instruction_index()
    condition_cb(ctx)
    jump_instr = ctx.next_instruction_index()
    ctx.emit(bytecode.JUMP_IF_FALSE)
    block_cb(ctx)
    ctx.emit(bytecode.JUMP_BACK, ctx.next_instruction_index()-start_instr)
    ctx.adjust_arg(jump_instr, ctx.next_instruction_index()-jump_instr-2)


def do_return(ctx):
    ctx.emit(bytecode.RETURN)


def load_none(ctx):
    cvar = ctx.register_constant(objectspace.TheNone)
    ctx.emit(bytecode.LOAD_CONSTANT, cvar)
    return cvar


def new_type(ctx, name, bases, code_cb):
    ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(objectspace.W_String(name)))
    for base_name in bases:
        load_var(ctx, base_name)
    build_tuple(ctx, len(bases))
    cctx = compilercontext.CompilerContext()
    code_cb(cctx)
    load_locals(cctx)
    do_return(cctx)
    code = cctx.create_bytecode()
    code_const = ctx.register_constant(code)
    ctx.emit(bytecode.LOAD_CONSTANT, code_const)
    ctx.emit(bytecode.MAKE_FUNCTION)
    ctx.emit(bytecode.CALL_FUNCTION, 0)
    ctx.emit(bytecode.MAKE_TYPE)


def enum(ctx, name, options, code_cb):
    # Make the base type
    new_type(ctx, name, ["Enum"], code_cb)
    for option in options:
        # dup the base type so we can setattr on it
        # and leave 1 on the top for the final
        # assignment to its name
        dup_top(ctx)
        # dup the base type for use in the bases
        dup_top(ctx)
        load_global(ctx, "Type")
        rot_two(ctx)
        load_global(ctx, "Enum")
        rot_two(ctx)
        if option.members:
            load_global(ctx, "Tuple")
            rot_two(ctx)
            build_tuple(ctx, 4)
        else:
            build_tuple(ctx, 3)
        ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(objectspace.W_String(option.name)))
        rot_two(ctx)
        ctx.emit(bytecode.LOAD_CONSTANT, ctx.register_constant(objectspace.W_Dict({})))
        ctx.emit(bytecode.MAKE_TYPE)
        if not option.members:
            # make a singleton
            ctx.emit(bytecode.CALL_FUNCTION, 0)
        set_attr(ctx, option.name)


def load_locals(ctx):
    ctx.emit(bytecode.LOAD_LOCALS)


def dup_top(ctx):
    ctx.emit(bytecode.DUP_TOP)


def rot_two(ctx):
    ctx.emit(bytecode.ROT_TWO)


def build_tuple(ctx, count):
    ctx.emit(bytecode.BUILD_TUPLE, count)


def is_(ctx):
    ctx.emit(bytecode.BINARY_IS)


def panic(ctx):
    ctx.emit(bytecode.PANIC)
