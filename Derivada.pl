%-------Diferenciación------%
derivate_polynomial(_, _,[],[]):-
    !.
derivate_polynomial(Degree, DerDegree, [Coef|Poly], [DerCoef | X]):-
    DerCoef is Coef*DerDegree,
    NewDerDegree is DerDegree +1,
    derivate_polynomial(Degree, NewDerDegree, Poly,  X).


%derivate(i,o)
derivate([_|Poly], X):-
    calcula_grado(Poly,DegreeDer),
    Degree is DegreeDer +1,
    derivate_polynomial(Degree, 1, Poly, X).

/*
Regla que calcula el grado de un polinomio pelón
*/

/*Recibe un polinomio "pelón" solo con coeficientes
Regresa el mismo polinomio pero con el grado pegado

agrega_grado(i,o).
*/

calcula_grado(Polinomio,Grado):-
    find_last_non_zero(Polinomio,Grado).



/* Caso base: determinamos que la posición es el último cero registrado
*/
find_last_non_zero([],_,LastNonZero,Position) :-
Position is LastNonZero,
!.


/*
Vamos vaciando la lista, cada que encontramos un cero
actualizamos la posición del último cero
*/
find_last_non_zero([X|Y],Count,LastNonZero,Position):-

(X =\= 0 ->  % Si encontramos un no cero
    NewCount is Count+1, %Seguimos la cuenta
    NewLNZ is Count, %Guardamos su posición como la del ultimo cero encontrado
    find_last_non_zero(Y,NewCount,NewLNZ,Position)
    ;
    %Si no hay cero pasamos el LastNonZero anterior
    NewCount is Count+1,
    find_last_non_zero(Y,NewCount,LastNonZero,Position)).

/*
Regla para encontrar el último no cero en una lista y regresar su posición

find_last_non_zero(i,o).
*/
find_last_non_zero(Lista,Position):-
    find_last_non_zero(Lista,0,0,Position).


/*
derivate_polynomial(Poly, [X]):-
    nth0(0, Poly, Degree),
    write(Degree),
    nth0(1, Poly, Polynomial),
    write(Polynomial),
    NewDegree is Degree - 1,
    derivate_polynomial(Degree, 0, Poly, X).
*/