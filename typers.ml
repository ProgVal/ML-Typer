(* Specification of typers module and adaptation of the different typers *)


module type GenTyper = sig
    type session
    type result
    type env
    type ast
    val infer : session -> env -> ast -> result
  end


module type BasicTyper = sig
    type ty
    include GenTyper
            with type session := Session.session
             and type result  := ty
             and type env      = ty Types.type_env
             and type ast      = Ast.basic_ast
end

module type BasicPolyTyper = BasicTyper with type ty = Types.ty_sch

module AdaptMonoTyper (T : BasicTyper with type ty = Types.ty)
         : BasicPolyTyper = struct
  type ty = Types.ty_sch
  type ast = Ast.basic_ast
  type env = ty Types.type_env
  let infer sess env ast =
    let mono_env = List.map (fun (str, sty) -> (str, Types.inst sty)) env in
    `TSTy (T.infer sess mono_env ast)
end



module Simple = AdaptMonoTyper (struct
                                 type ty = Types.ty
                                 type env = ty Types.type_env
                                 type ast = Ast.basic_ast
                                 include Typers_simple
                               end)

module Core = AdaptMonoTyper (struct
                               type ty = Types.ty
                               type env = ty Types.type_env
                               type ast = Ast.basic_ast
                               include Typers_core
                             end)

module Poly : BasicPolyTyper = struct
  type ty = Types.ty_sch
  type ast = Ast.basic_ast
  type env = ty Types.type_env
  include Typers_poly
end

