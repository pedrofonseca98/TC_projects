let lcs a b m n =
  let subMatrix = Array.make_matrix (m + 1) (n + 1) 0 in (*inicializa a matriz*)
    let result = ref 0 in (*declaração da variável para retorno (tamanho da maior substring comum)*)
    for i = 0 to m do (*itera sobre a*)
      for j = 0 to n do (*itera sobre b*)
        if i = 0 || j = 0 then subMatrix.(i).(j) <- 0 (*inicializa a primeira linha e coluna da matriz com o valor 0*)
        else if a.[i - 1] = b.[j - 1] then begin (*verifica se as letras das posições são iguais, no caso do exemplo a baixo, a execussão entra neste passo para i = 2 j = 2 e i = 3 j = 3*)
          subMatrix.(i).(j) <- subMatrix.(i - 1).(j - 1) + 1; (*se sim, soma +1 ao valor anterior da diagonal*)
          if !result > subMatrix.(i).(j) then result:= !result (*se o resultado for maior que o atual da matriz, mantem-se*)
          else result:= subMatrix.(i).(j) (*o valor resultado é substituido, se o atual da matriz for superior, no caso do exemplo a baixo, a execussão entra neste passo para i = 2 j = 2 e i = 3 j = 3*)
        end
        else subMatrix.(i).(j) <- 0 (*caso nenhuma condição anterior se verifique, a posição i j toma o valor 0, no caso do exemplo a baixo, a execussão entra neste passo para todos os retantes valores de i e j*)
      done;
    done;

    !result (*retorna o result, que é o valor mais elevado da matriz*)


let () =
  let (a:string) = read_line() in
  let (b:string) = read_line() in
  let lenA = String.length(a) in
  let lenB = String.length(b) in
    print_endline(string_of_int (lcs a b lenA lenB))



(*
https://www.youtube.com/CodingMadeSimple
https://algorithms.tutorialhorizon.com/dynamic-programming-longest-common-substring/

Este algoritmo é baseado em programação dinâmica e usa o preenchimento de uma matriz
para determinar a maior subsequencia contigua entre 2 strings. O algoritmo resolve o
problema em  O(m*n) tempo.

X= abc    Y= zbc

       |  | a | b | c |
    _
        0   0   0   0
    _
    z   0   0   0   0
    _
    b   0   0   1   0
    _
    c   0   0   0   2
    _

Tal que, se: X[i]==Y[j], então: m[i][j] = m[i-1][j-1] + 1

Resultado = max(matrix) = 2

A matriz é preenchida de cima para baixo, da esquerda para a direita

*)
