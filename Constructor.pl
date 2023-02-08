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

create_polynomial(Degree,Coef,[Degree|Poly]):-
    NewDegree is Degree-1, /*Ajustamos el degree para no poner ceros de mas*/
    add_zeros(NewDegree,Coef,Poly).