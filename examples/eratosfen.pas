(* Целые числа от 2 до 100: решето Эратосфена *)
_program main;
_var
    prime: _array [2..100] _of boolean;
    n, k: integer;
_(
    (* Обнулим prime *)
    _for n:=2 _to 100 _do
        prime [n] := true;

    (* Вычислим prime *)
    _for n:=2 _to 100 _do
        _if prime[n] _then
            _for k:=2 _to trunc (100/n) _do
                prime [n*k] := false;

    (* Печать prime *)
    _for n:=2 _to 100 _do
        _if prime[n] _then
            write (n);
    writeln;
_).
