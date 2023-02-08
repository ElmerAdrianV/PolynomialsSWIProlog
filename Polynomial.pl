% To-Do hacer que el create_polynomial no se rompa con negativos

%base case
% Regresamos como cola de la lista el Coef
add_zeros(-1,Coef,[Coef]):-
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

/*Recibe un polinomio "pelón" solo con coeficientes
Regresa el mismo polinomio pero con el grado pegado
agrega_grado(i,o).

*/
agrega_grado(Polinomio, Grado):-
    find_last_non_zero(Polinomio,Grado).


/*MULTIPLICACION DE POLINOMIOS*/

/*
Recibe dos polinomios con su grado
los multiplica y luego etiqueta con su
grado al polinomio resultante
multiplica_polinomios(i,i,o)
*/
multiplica_polinomios(Polinomio1,Polinomio2,PolinomioFinal]):-
        multiplica_recursivo(Polinomio1,Polinomio2,PolinomioFinal),
        calcula_grado(PolinomioFinal,GradoFinal). %Añadimos grado y regresamos



/*Caso base*/
multiplica_recursivo([],_,[]).

multiplica_recursivo([X|Y],Pol2,[Prod|Final]):-
  /*Multiplicamos la cabeza del primer polinomio por todo el segundo
  Guardamos el resultado en 2 partes: Prod que ya no se toca
  y Demas, que sumaremos con las multiplicaciones siguientes*/
   
   multiplica_monomio(X,Pol2,[Prod|Demas]),

  /*Repetimos sobre la cola del polinomio*/
   
   multiplica_recursivo(Y,Pol2,Regreso),

  /*Sumamos Demás con lo que nos regresó la llamada recursiva
  Vamos sumando "para atras"*/

   suma_recursiva(Regreso,Demas,Final).

/*
Método auxiliar para multiplicar un monomio por
todo otro polinomio
multiplica_monomio(i,i,o)
*/
multiplica_monomio(_,[],[]):-
   !.
multiplica_monomio(Monomio,[X|Y],[Resultado|Resto]):-
   Resultado is Monomio*X,
   multiplica_monomio(Monomio,Y,Resto).


/*Método auxiliar para sumar polinomios, va aparte*/
/*SUMA DE POLINOMIOS*/
suma_recursiva([],Lista2, Lista2):- !.

suma_recursiva(Lista1,[],Lista1) :-
!.

suma_recursiva([X|Y],[Z|W],[Resultado|Resto]):-
Resultado is X+Z,
suma_recursiva(Y,W,Resto).

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



%-------Diferenciación------%

derivate_polynomial(Degree, Degree,[],[]):-
    !.
derivate_polynomial(Degree, DirDegree, [Coef|Poly], [DirCoef | X]):-
    Mult is DirDegree-1,
    DirCoef is Coef*Mult,
    NewDirDegree is DirDegree +1,
    derivate_polynomial(Degree, NewDirDegree, Poly,  X)
.

derivate_polynomial([Degree|Poly], [NewDegree|[X]]):-
    NewDegree is Degree - 1,
    write(NewDegree),
    derivate_polynomial(Degree, 0, Poly, X).
obtenGrado([Degree|[_]], Degree):-
    !.

/*
derivate_polynomial(Poly, [NewDegree|[X]]):-
    nth0(0, Poly, Degree),
    write(Degree),
    nth0(1, Poly, Polynomial),
    write(Polynomial),
    NewDegree is Degree - 1,
    derivate_polynomial(Degree, 0, Poly, X).
*/

