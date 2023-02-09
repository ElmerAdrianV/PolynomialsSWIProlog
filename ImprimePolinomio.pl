write_polynomial(Degree, I,[Coef|Poly]):-
        %Solo imprime coeficientes diferentes de 0
        (	Coef=\=0 ->
            %imprime el signo del coeficientes
            (	Coef<0 ->
                %Vemos si escribir o no el uno
                (Coef =\= -1 ->
                    write(Coef);
                    (   I=:=0 ->
                        write(Coef)
                        ;
                        write("-")
                    ) 
                )
            ;
                (Degree =\= I ->
                    write("+");
                    write("")
                )   
            ,
                %Vemos si escribir o no el uno
                (Coef=\=1->
                    write(Coef);
                    (   I=:=0 ->
                        write(Coef)
                        ;
                        write("")
                    ) 
                )
            ),
            ( I =\= 0 ->
                write("X"),
                (
                    I=\=1 ->
                        write("^"),write(I);
                        write("")
                )
                ;
                write("")
            )
            ;
            write("")
	    )
    ,
    NewI is I-1,
    write_polynomial(Degree,NewI,Poly).

write_polynomial(_,-1,[]):-
    nl,!.

write_polynomial(Poly):-
    calcula_grado(Poly, Degree),
    reverse(Poly, NewPoly),
    write_polynomial(Degree, Degree,NewPoly).

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
