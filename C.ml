open Array
open List
open Str
open Scanf
open Printf

type regexp =
 | V
 | E
 | C of char
 | U of regexp * regexp
 | P of regexp * regexp
 | S of regexp

module Parser_regexp = struct


module MenhirBasics = struct

  exception Error

  type token =
    | RPAREN
    | LPAREN
    | EPS
    | EOF
    | EMP
    | CONC
    | CHAR of (
       (char)
  )
    | AST
    | ALT

end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state =
  | MenhirState12
  | MenhirState7
  | MenhirState6
  | MenhirState1
  | MenhirState0


let rec _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr =
                                ( P (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)) : 'freshtv48)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state * 'tv_term)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_term)), _, (e2 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr =
                                ( U (e1, e2) )
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)) : 'freshtv52)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_atom =
                                ( e )
             in
            _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv54)) : 'freshtv56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv69 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (le : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
       (regexp)
            ) =
                                ( le )
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
       (regexp)
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
       (regexp)
            )) : (
       (regexp)
            )) = _v in
            (Obj.magic _1 : 'freshtv62)) : 'freshtv64)) : 'freshtv66)) : 'freshtv68)) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv75)

and _menhir_goto_term : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState12 | MenhirState6 | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ALT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv32)
        | CONC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CHAR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | EMP ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | EPS ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv34)
        | EOF | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_term)) = _menhir_stack in
            let _v : 'tv_expr =
                                ( e )
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * 'tv_factor) * _menhir_state * 'tv_term) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (e1 : 'tv_factor)), _, (e2 : 'tv_term)) = _menhir_stack in
        let _v : 'tv_term =
                                ( P (e1, e2) )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv42)) : 'freshtv44)

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_factor -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ALT | CONC | EOF | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_factor)) = _menhir_stack in
        let _v : 'tv_term =
                                ( e )
         in
        _menhir_goto_term _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv30)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atom -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_factor =
                                ( S e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv20)) : 'freshtv22)
    | ALT | CHAR _ | CONC | EMP | EOF | EPS | LPAREN | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_atom) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (e : 'tv_atom)) = _menhir_stack in
        let _v : 'tv_factor =
                                ( e )
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v) : 'freshtv24)) : 'freshtv26)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * 'tv_factor) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state * 'tv_term)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv18)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom =
                                ( E )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atom =
                                ( V )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
       (char)
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
       (char)
    )) : (
       (char)
    )) = _v in
    ((let _v : 'tv_atom =
                                ( C c )
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and regexpr : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
       (regexp)
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHAR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EMP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EPS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))





end

module Lexer_regexp = struct

  open Parser_regexp

  exception Error of string


  let __ocaml_lex_tables = {
    Lexing.lex_base =
    "\000\000\245\255\246\255\247\255\248\255\249\255\250\255\251\255\
      \252\255\253\255\254\255\255\255";
    Lexing.lex_backtrk =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255";
    Lexing.lex_default =
    "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000";
    Lexing.lex_trans =
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \004\000\003\000\007\000\009\000\000\000\000\000\008\000\000\000\
      \006\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
      \010\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \002\000";
    Lexing.lex_check =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000\000\000\000\000\000\000\255\255\255\255\000\000\255\255\
      \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
      \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
      \000\000";
    Lexing.lex_base_code =
    "";
    Lexing.lex_backtrk_code =
    "";
    Lexing.lex_default_code =
    "";
    Lexing.lex_trans_code =
    "";
    Lexing.lex_check_code =
    "";
    Lexing.lex_code =
    "";
  }

  let rec tokenize lexbuf =
    __ocaml_lex_tokenize_rec lexbuf 0
  and __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state =
    match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
        | 0 ->
                                        ( tokenize lexbuf )

    | 1 ->
  let
                          s
  = Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
                                        ( CHAR s )

    | 2 ->
                                        ( ALT )

    | 3 ->
                                        ( CONC )

    | 4 ->
                                        ( AST )

    | 5 ->
                                        ( EMP )

    | 6 ->
                                        ( EPS )

    | 7 ->
                                        ( LPAREN )

    | 8 ->
                                        ( RPAREN )

    | 9 ->
                                        ( EOF )

    | 10 ->
        ( raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) )

    | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
        __ocaml_lex_tokenize_rec lexbuf __ocaml_lex_state

  ;;
end

(* --------------------------------------------------------------------------------------------------------------- *)
(* Comecar aqui*)
let stateCounter = ref 0 (*número do estado*)

type state = (*cada estado tem um id*)
  {
    id:int;
  }

type trans = (*cada transição tem 2 estados e um simbolo; & -> epsilon*)
  {
    fromState:state;
    symbol:char;
    toState:state
  }

let createState id1 = (*cria um estado*)
  {
    id = id1;
  }

let createTrans state1 c state2 = (*cria uma transição*)
  {
    fromState = state1;
    symbol = c;
    toState = state2
  }

let epsTrans =
  let a = createState (!stateCounter) in (*cria primeiro estado*)
  let b = createState (!stateCounter + 1) in (*cria segundo estado*)
  let t = Array.make 1 (createTrans a '&' b) in (*cria transição eps entre os estados*)
  t

let emptyAutomata = epsTrans  (*ou deve fazer print 'Empty Automata!' ???*)

let charTrans symbol =
  let a = createState (!stateCounter) in (*cria primeiro estado*)
  let b = createState (!stateCounter + 1) in (*cria segundo estado*)
  stateCounter:= !stateCounter + 2; (*incrementea a variável que atribui os id aos estados*)
  let t = Array.make 1 (createTrans a symbol b) in (*cria transição char entre os estados*)
  t

let union g f =
  let a = createState (!stateCounter) in (*novo estado inicial*)
  let b = createState (!stateCounter + 1) in (*novo estado final*)
  stateCounter:= !stateCounter + 2; (*incrementea a variável que atribui os id aos estados*)
  let t = Array.append f g in (*junta as transiçãoes de f e g*)
  let c = f.(0) in (*primeira transição de f*)
  let d = g.(0) in (*primeira transição de g*)
  let cc = f.((Array.length f) - 1) in (*ultima transição de f*)
  let dd = g.((Array.length g) - 1) in (*ultima transição de g*)
  let t =  Array.append (Array.make 1 (createTrans a '&' c.fromState)) t in (*junta a transição do novo estado inicial ao estado inicial de f*)
  let t = Array.append (Array.make 1 (createTrans a '&' d.fromState)) t in (*junta a transição do novo estado inicial ao estado inicial de g*)
  let t = Array.append t (Array.make 1 (createTrans cc.toState '&' b)) in (*junta a transição do antigo estado final de f novo estado final*)
  let t = Array.append t (Array.make 1 (createTrans dd.toState '&' b)) in (*junta a transição do antigo estado final de g novo estado final*)
  t (*retorna o array das transições*)

let concat g f =
  let a = f.((Array.length f) - 1) in (*ultima transição de f*)
  let b = g.(0) in (*primeira transição de f*)
  (*
  let trans = createTrans a.toState '&' b.fromState in
  let t = Array.append f (Array.append (Array.make 1 trans g) in
  *)
  let t = Array.append f (Array.append (Array.make 1 (createTrans a.toState '&' b.fromState)) g) in (*cria array com as transiçoes f trans g*)
  t (*retorna o array das transições*)

  let closure s =
    let a = createState (!stateCounter) in (*novo estado inicial*)
    let b = createState (!stateCounter + 1) in (*novo estado final*)
    stateCounter:= !stateCounter + 2; (*incrementea a variável que atribui os id aos estados*)
    let c = s.(0) in (*primeira transição de s*)
    let d = s.((Array.length s) - 1) in (*ultima transição de s*)
    let s = Array.append (Array.make 1 (createTrans a '&' b)) s in (*junta a transição do novo estado inicial de s novo estado final*)
    let s = Array.append (Array.make 1 (createTrans a '&' c.fromState)) s in (*junta a transição do novo estado inicial ao antigo estado inicial de s*)
    let s = Array.append s (Array.make 1 (createTrans d.toState '&' c.fromState)) in (*junta a transição do antigo estado final de s ao antigo estado inicial de s*)
    let s = Array.append s (Array.make 1 (createTrans d.toState '&' b)) in (*junta a transição do antigo estado final de s ao novo estado final*)
    s

let rec toAutomata s = (*função "main" da constução do autómato*)
  match s with
  | V       -> emptyAutomata
  | E       -> epsTrans (*transição eps*)
  | C  c    -> charTrans c (*transição char*)
  | U (f,g) -> union (toAutomata g) (toAutomata f) (*união*)
  | P (f,g) -> concat (toAutomata g) (toAutomata f) (*concatenação*)
  | S s     -> closure (toAutomata s) (*fecho de Kleene*)


let rec addEpstoList nfa n state endState visited =
  let a = ref (Array.make 0 0) in (*contém os estados adicionados apartir de state*)
  if Array.mem state !visited = true then !a (*se o estado atual já foi "visitado" não é necessário voltar a adicionar*)
  else begin (*se o estado atual ainda não foi visitado*)
    visited:= Array.append !visited (Array.make 1 state); (*adiciona o estado atual aos visitados*)
    if state = endState then begin a:=(Array.make 1 state); !a end (*se o estado for i final, devolve-o, pois não há transições a sair do estado final*)
    else begin (*se não for estado final*)
      for i=0 to n-1 do (*precorre o autómato*)
        if nfa.(i).fromState.id = state then begin (*quando encontra o estado atual nos estados de partida do autómato*)
          if nfa.(i).symbol <> '&' then a:= Array.append !a (Array.make 1 (nfa.(i).fromState.id)) (*se a transição for diferente de espilon, adiciona o estado a 'a'*)
          else begin a:= Array.append !a (addEpstoList nfa n (nfa.(i).toState.id) endState visited) end (*se não, precorre as transições epsilon do estado atual até chegar ao final ou até um estado de partida com transição diferente de epsilon*)
        end
      done;
      !a (*retorna a que contem os estados que foram atingidoa apartir de state*)
    end
  end

let search nfa n word startState endState =
  let curStates = ref (Array.make 0 0) in (*guarda os estados a procurar na iteração presente*)
  let nextStates = ref (Array.make 0 0) in (*guarda os estados a procurar na iteração seguinte*)
  let added = ref (Array.make 0 0) in (*guarda os estados que já foram adicionados iteração presente, para não serem readicionados*)
  curStates:=Array.append !curStates (addEpstoList nfa n startState endState (ref (Array.make 0 0))); (*adiciona o estado inicial do autómato*)
  for i=0 to (String.length word) -1 do (*precorre a palavra*)
    nextStates:=Array.make 0 0; (*faz reset no inicio da iteração*)
    added:=Array.make 0 0; (*faz reset no inicio da iteração*)
    for k=0 to (Array.length !curStates) - 1 do (*precorre os estados que foram atingidos na iteração anterior*)
      for j=0 to n-1 do (*precorre as transições do autómato*)
        if nfa.(j).fromState.id = !curStates.(k) && (nfa.(j).symbol = word.[i] || nfa.(j).symbol = 'z' || nfa.(j).symbol = '&') && Array.mem nfa.(j).fromState.id !added = false then begin (*se o estado de partida da transição pertencer a curStates e se a transição for "letra" z ou eps, avança*)
          added:= Array.append !added (Array.make 1 nfa.(j).fromState.id); (*o estado atual é adicionado aos "adicionados"*)
          nextStates:=Array.append !nextStates (addEpstoList nfa n (nfa.(j).toState.id) endState (ref (Array.make 0 0))) (*o estado atual é adicionado aos "próximos estados" com auxilio da função addEpstoList*)
        end
      done;
    done;
    curStates:= !nextStates; (*atualiza dos novos estados atuais a serem precorridos na iteração seguinte*)
    for i=0 to (Array.length !curStates) - 1 do (*caso uma substring já tenha sido encontrada a resposta é YES e é escusado continuar a procurar, como tal o programa pára*)
      if !curStates.(i) = endState then begin
        print_endline("YES");
        exit 0
      end
      (*se ainda não foi encontrada uma substring o processo anterior repete-se até ser encontrada uma substring ou até a palavra ser toda consumida*)
    done;
  done;
  for i=0 to (Array.length !curStates) - 1 do
    if !curStates.(i) = endState then begin (*se a palavra for toda consumida verifica se a última "pesquisa" deu resultado positivo*)
      print_endline("YES");
      exit 0
    end
  done;
  print_endline("NO"); (*se no final da execução não houve substrings positivas a resposta é NO*)


open Parser_regexp

let regexp st =
  let linebuf = Lexing.from_string st in
  try regexpr Lexer_regexp.tokenize linebuf
  with _ -> failwith "regexp: input problem"

let rec string_of_regexp s =
  match s with
  | V       -> "0"
  | E       -> "1"
  | C  c    -> String.make 1 c
  | U (f,g) -> "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | P (f,g) -> "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | S s     -> (string_of_regexp s)^"*"


let regExpSrtring =
  let (exp_input:string) = "z*("^read_line()^")" in (*z é um simbolo especial que serve para lidar com as substring, pois aceita qualquer letra
                                                      a expressão z*.(regexp) lida com substrings*)
  let r = regexp exp_input in
  let word = read_line() in (*string a procurar*)
  let nfa = toAutomata r in (*nfa é o array de todas as transições do automato*)
  let size = Array.length nfa in
  search nfa size word (nfa.(0).fromState.id) (nfa.(size-1).toState.id); (*chama a função de pesquisa*)




(*
https://deniskyashif.com/2019/02/17/implementing-a-regular-expression-engine/
http://www.di.ubi.pt/~desousa/TC/tcomp.html
emails de esclarecimentos de dúvidas

#versão final
*)
