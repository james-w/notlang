from . import bytecode, compilercontext, objectspace


def load_var(ctx, name):
    if ctx.is_local(name):
        op = bytecode.LOAD_VAR
    else:
        op = bytecode.LOAD_GLOBAL
    vnum = ctx.register_var(name)
    ctx.emit(op, vnum)
    return vnum


def load_constant_int(ctx, val):
    vnum = ctx.register_constant(objectspace.W_Int(val))
    ctx.emit(bytecode.LOAD_CONSTANT, vnum)
    return vnum


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


def function_call(ctx, name, numargs, args_cb):
    fnum = load_var(ctx, name)
    args_cb(ctx)
    ctx.emit(bytecode.CALL_FUNCTION, numargs)
    return fnum


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
