%base case
% Regresamos como cola de la lista el Coef

add_zeros(-1,Coef,Coef):-
    !.

/*Caso no base, agregamos un cero y entre el grado y el futuro Coef
Este es el método que se llama recursivamente*/

add_zeros(Degree,Coef,[0|Poly]):-
    NewDegree is Degree - 1,
    add_zeros(NewDegree,Coef,Poly).

/* create_polynomial(i,i,o) */
%Le pegamos el degree hasta adelante desde el inicio%
%Poly serán los ceros + el coef

create_polynomial(Degree,Coef,Poly):-
    NewDegree is Degree-1, /*Ajustamos el degree para no poner ceros de mas*/
    add_zeros(NewDegree,Coef,Poly).

%-------Diferenciación------%
derivate_polynomial(_, _,[],[]):-
    !.
derivate_polynomial(Degree, DirDegree, [Coef|Poly], [DirCoef | X]):-
    Mult is DirDegree,
    DirCoef is Coef*Mult,
    NewDirDegree is DirDegree +1,
    derivate_polynomial(Degree, NewDirDegree, Poly,  X).

derivate_polynomial(_, 0, [_|Poly],  [_|X]):-
    derivate_polynomial(_, 1, Poly,  X).

derivate([_|Poly], X):-
    calcula_grado(Poly,DegreeDer),
    Degree is DegreeDer +1,
    derivate_polynomial(Degree, 1, Poly, X).
derivate().
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
