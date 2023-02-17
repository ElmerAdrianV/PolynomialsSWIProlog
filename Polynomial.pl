%Integrantes del Equipo

% Elmer Adrián Ortega Valdés. CU: 197032
% César Dario Sotelo Aportela. CU: 197032
% Diego Arellano Zamudio. CU: 198002
% José Ulises Quevedo Llergo. CU: 197032

/*********Clase de polinomios**********/
/*********Constructor de polinomio*****/
%Caso base
% Regresamos como cola de la lista el Coef
agrega_ceros(-1,Coef,[Coef]):-
    !.

/*Caso no base, agregamos un cero y entre el grado y el futuro Coef
Este es el predicado que se llama recursivamente*/
agrega_ceros(Grado,Coef,[0|Poly]):-
    NuevoGrado is Grado - 1,
    agrega_ceros(NuevoGrado,Coef,Poly).

/* crea_polinomio(i,i,o) */
%Le pegamos el Grado hasta adelante desde el inicio%
%Poly serán los ceros + el coef

crea_polinomio(Grado,Coef,Poly):-
    NuevoGrado is Grado-1, /*Ajustamos el Grado para no poner ceros de mas*/
    agrega_ceros(NuevoGrado,Coef,Poly).


/*********Imprime de polinomio*****/
/*
Este predicado imprime los polinomios de forma en su representación
matemática habitual, es decir, coeficientes 1 o términos con 0, no aparecen
*/

%escribe polinomio(i,i,i)
escribe_polinomio(Grado, I,[Coef|Poly]):-
    %Solo imprime coeficientes diferentes de 0
    (	Coef=\=0 ->
        %imprime el signo del coeficientes
        (	Coef<0 ->
            (Grado =:= I ->
                (
                    %Vemos si escribir o no el coeficiente
                    (Coef =\= -1 ->
                        write(Coef);
                        %checa si el coeficiente es el termino constante
                        (   I=:=0 ->
                            AbsCoef is abs(Coef),
                            write(" - "),
                            write(AbsCoef);
                            write("")
                        )
                    )
                )
                ;
                %Vemos si escribir o no el coeficiente
                (Coef =\= -1 ->
                    AbsCoef is abs(Coef),
                    write(" - "),
                    write(AbsCoef);
                    %checa si el coeficiente es el termino constante
                    (   I=:=0 ->
                        AbsCoef is abs(Coef),
                        write(" - "),
                        write(AbsCoef);
                        write("")
                    )
                )
            )
        ;
            %se asegura de imprimir o no el signo del coeficiente
            (Grado =\= I ->
                write(" + ");
                write("")
            )
        ,
            %Vemos si escribir o no el coeficiente
            (Coef=\=1->
                write(Coef);
                %checa si el coeficiente es el termino constante
                (   I=:=0 ->
                    write(Coef)
                    ;
                    write("")
                )
            )
        ),
        %se asegura de como imprimir la incognita y su coeficiente
        ( I =\= 0 ->
            write("x"),
            (
                I=\=1 ->
                    write("^"),write(I);
                    write("")
            )
            ;
            write("")
        )
        ;
        (
            %imprime el polinomio 0
            Grado=:=0 ->
                write(Coef);
                write("")
        )
    ),
    NuevoI is I-1,
    escribe_polinomio(Grado,NuevoI,Poly).

% Caso base de escribe polinomio
%escribe_polinomio(i,i,i)
escribe_polinomio(_,-1,[]):-
    nl,!.

%predicado que imprime el polinomio (representado en lista) y lo imprime
%simbolicamente
%escribe_polinomio(i)
escribe_polinomio(Poly):-
    calcula_grado(Poly, Grado),
    reverse(Poly, NuevoPoly),
    escribe_polinomio(Grado, Grado,NuevoPoly).

/*********Evalua polinomio**********/
%%predicado que evalua el polinomio de forma secuencial

%caso base
%evalua_polinomio(i,i,i,o)
evalua_polinomio([],_,_,Resultado):-
   Resultado is 0,
   !.
%predicado auxiliar
%evalua_polinomio(i,i,i,o)
evalua_polinomio([X|Y],Grado,Valor,Resultado):-
    Vari is Valor**Grado,
    Termino is Vari*X,
    Next_grad is Grado + 1,
    evalua_polinomio(Y,Next_grad,Valor,Res2),
    Resultado is Termino + Res2.
%predicado principal
%evalua_polinomio(i,i,o)
evalua_polinomio(Polinomio,Valor,Resultado):-
    evalua_polinomio(Polinomio,0,Valor,Resultado).

/*********Suma de polinomio*****/
%Primero tenemos el caso base para cuando ya nos acabamos los elementos de la lista que queremos sumar.
%suma(i,i,o).
suma(Poli1,Poli2,Polir):-
	suma_aux(Poli1,Poli2,PolAux),
    limpia_polinomio(PolAux,Polir).
%caso base
suma_aux([],[],[]):-
    !.
%Después hacemos el caso donde son del mismo tamaño.
suma_aux([X|Poli1],[Y|Poli2],[Z|Polir]):-
	Z is X+Y,
	suma_aux(Poli1,Poli2,Polir).

%Estas dos partes adicionales las ponemos para cuando los polinomios son de diferente tamaño.
suma_aux([],Poli2,Poli2):-
	!.
suma_aux(Poli1,[],Poli1):-
	!.

/*********Resta de polinomio*****/

%resta(i,i,o).
resta(Poli1,Poli2,Polir):-
	resta_aux(Poli1,Poli2,PolAux),
    limpia_polinomio(PolAux,Polir).
%El caso de las restas es igual, solo que el Z is cambia a X-Y
%casos base
resta_aux([],[],[]):-
    !.
resta_aux(Poli1,[],Poli1):-
	!.


%Después hacemos el caso donde son del mismo tamaño.
resta_aux([X|Poli1],[Y|Poli2],[Z|Polir]):-
	Z is X-Y,
	resta_aux(Poli1,Poli2,Polir).

%Estas es una parte adicional que las ponemos para cuando
%los polinomios son de diferente tamaño.
resta_aux([],[Y|Poli2],[Z|Polir]):-
	Z is 0-Y,
	resta_aux([],Poli2,Polir).


/*********Multiplicación de polinomio*****/

/*
Recibe dos polinomios sin grado,
los multiplica y regesa el polinomio
resultante (sin su grado)
multiplica_polinomios(i,i,o)
*/
multiplica_polinomios(Polinomio1,
                      Polinomio2,
                      PolinomioFinal):-
        multiplica_recursivo(Polinomio1,Polinomio2,PolinomioFinal).



/*Caso base*/
multiplica_recursivo([],_,[]):-
  !.

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


/* predicado auxiliar para multiplicar un monomio por
todo otro polinomio
multiplica_monomio(i,i,o)
*/
multiplica_monomio(_,[],[]):-
   !.
multiplica_monomio(Monomio,[X|Y],[Resultado|Resto]):-
   Resultado is Monomio*X,
   multiplica_monomio(Monomio,Y,Resto).


/*predicado auxiliar para sumar polinomios, va aparte*/
/*SUMA DE POLINOMIOS*/
suma_recursiva([],Lista2, Lista2):-
  !.
suma_recursiva(Lista1,[],Lista1) :-
  !.
suma_recursiva([X|Y],[Z|W],[Resultado|Resto]):-
  Resultado is X+Z,
  suma_recursiva(Y,W,Resto).



/*********Derivación de polinomio*****/
%caso base del predicado deriva con una lista vacía
%deriva_polinomio(i,i,i,o)
deriva_polinomio(_, _,[],[]):-
    !.


%predicado que calcula la derivada del i-esimo término
%deriva_polinomio(i,i,i,o)
deriva_polinomio(Grado, DerGrado, [Coef|Poly], [DerCoef | X]):-
    DerCoef is Coef*DerGrado,
    NuevoDerGrado is DerGrado + 1,
    deriva_polinomio(Grado, NuevoDerGrado, Poly,  X).

%predicado que recibe la instrucción de derivar
%un polinomio representando en lista
%deriva(i,o)
deriva([_|Poly], X):-
    calcula_grado(Poly,GradoDer),
    Grado is GradoDer +1,
    deriva_polinomio(Grado, 1, Poly, X).

/*********Composición de polinomio*****/


% Predicado que realiza la comoposición de polinomios por el método de Horner
% Composicion de polinomios (i,i,o  )
composicion_polinomios(X,Pol2,PolFinal):-
	reverse(X,R),
	crea_polinomio(0,0,PolAux),
	composicion_polinomios(R,Pol2,PolAux,AuxPolFinal),
	limpia_polinomio(AuxPolFinal,PolFinal).

% Caso base del predicado
% Composicion de polinomios (i,i,o )
composicion_polinomios([],_,NewPolAux,NewPolAux):-
	!.

% Predicado auxiliar
% Composicion de polinomios (i,i,i,o )
composicion_polinomios([X|Y], Pol2, PolAux,PolFinal):-
	crea_polinomio(0,X,Term),
	multiplica_recursivo(Pol2,PolAux,M),
	suma_recursiva(Term,M,NewPolAux),
	composicion_polinomios(Y,Pol2,NewPolAux,PolFinal).

/*********predicados auxiliares*****/

%%Limpia polinomio
%%metodo que elimina los terminos con coeficiente 0 y que estan
%en una posición mayor que el grado del polinomio
%se apoya del predicado calcula_grado

%caso base
%limpia_polinomio(i,i,o)
limpia_polinomio(_,-1,[]):-
    !.

%limpia_polinomio(i,i,o)
limpia_polinomio([Coef|Poly],Grado,[Coef|NuevoPoly]):-
    NuevoGrado is Grado-1,
    limpia_polinomio(Poly,NuevoGrado, NuevoPoly).

%limpia_polinomio(i,o)
limpia_polinomio(Poly,NuevoPoly):-
    calcula_grado(Poly,Grado),
    limpia_polinomio(Poly,Grado, NuevoPoly).

/*
Regla que calcula el grado de un polinomio pelón
*/

/*Recibe un polinomio "pelón" solo con coeficientes
Regresa el mismo polinomio pero con el grado pegado

agrega_grado(i,o).
*/

calcula_grado(Poly,Grado):-
    encuentra_ultimo_no_cero(Poly,Grado).

/* Caso base: determinamos que la posición es el último cero registrado
*/
encuentra_ultimo_no_cero([],_,UltimoNoCero,Posicion) :-
Posicion is UltimoNoCero,
!.


/*
Vamos vaciando la lista, cada que encontramos un cero
actualizamos la posición del último cero
*/
encuentra_ultimo_no_cero([X|Y],Count,UltimoNoCero,Posicion):-

(X =\= 0 ->  % Si encontramos un no cero
    NuevoCount is Count+1, %Seguimos la cuenta
    NuevoLNZ is Count, %Guardamos su posición como la del ultimo cero encontrado
    encuentra_ultimo_no_cero(Y,NuevoCount,NuevoLNZ,Posicion)
    ;
    %Si no hay cero pasamos el UltimoNoCero anterior
    NuevoCount is Count+1,
    encuentra_ultimo_no_cero(Y,NuevoCount,UltimoNoCero,Posicion)).

/*
Regla para encontrar el último no cero en una lista y regresar su posición
encuentra_ultimo_no_cero(i,o).
*/
encuentra_ultimo_no_cero(List,Posicion):-
    encuentra_ultimo_no_cero(List,0,0,Posicion).


%% predicado main donde se corren las pruebas    
main :-
    crea_polinomio(0,0,Zero),
    crea_polinomio(3,4,P1),
    crea_polinomio(2,3,P2),
    crea_polinomio(0,1,P3),
    crea_polinomio(1,2,P4),

    suma(P1,P2,P_temp),suma(P_temp,P3,P_temp2),suma(P_temp2,P4,P),

    crea_polinomio(2,3,Q1),
    crea_polinomio(0,5,Q2),
    suma(Q1,Q2,Q),

    suma(P,Q,R),
    multiplica_polinomios(P,Q,S),
    composicion_polinomios(P,Q,T),


      /*Zona de prints*/

      write("zero(x)     = "), escribe_polinomio(Zero),
      write("p(x)        = "),   escribe_polinomio(P),
      write("q(x)        = "), escribe_polinomio(Q),
      write("p(x) + q(x) = "), escribe_polinomio(R),
      write("p(x) * q(x) = "), escribe_polinomio(S),
      write("p(q(x))     = "),escribe_polinomio(T),
      write("0 - p(x)    = "),resta(Zero,P,Resta_temp),escribe_polinomio(Resta_temp),
      write("p(3)        = "),evalua_polinomio(P,3,Resultado_eval),write(Resultado_eval),nl,
      write("p'(x)       = "), deriva(P,Derivada1),escribe_polinomio(Derivada1),
      write("p''(x)      = "), deriva(Derivada1,Derivada2),escribe_polinomio(Derivada2),
      !.