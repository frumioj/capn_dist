-module(crypto_power).
-export([mod/2, pow/3]).

mod(0,_) -> 0;
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 ->
    K = (-X div Y)+1,
    PositiveX = X+K*Y,
    PositiveX rem Y.

pow(1,0,_) -> 1;
pow(0,1,_) -> 0;
pow(_,_,1) -> 0;
pow(Base, Exponent, Modulus) when Base > 0 ->
    binary:decode_unsigned(crypto:mod_pow(Base, Exponent, Modulus));
pow(Base, Exponent, Modulus) when Base < 0 ->
    case 1 band Exponent of
	1 ->
	    modular_pow(Base, Exponent, Modulus, Base);
	0 ->
	    modular_pow(Base, Exponent, Modulus, 1)
    end.

modular_pow(_Base, 0, _Modulus, Return) -> Return;
modular_pow(Base, Exponent, Modulus, Return) ->
    E2 = Exponent bsr 1,
    B2 = mod(Base*Base, Modulus),
    case E2 band 1 of
	1 ->
	    modular_pow(B2, E2, Modulus, mod(Return*B2, Modulus));
	_ ->
	    modular_pow(B2, E2, Modulus, Return)
    end.
