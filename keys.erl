-module(keys).
-export([new/0]).

%% generate 32 random bytes
%% and then apply the curve25519 "clamp"
%% using bit manipulations
new() ->
    PrePrivate = crypto:strong_rand_bytes(32),

    %% match out the first byte and the 31st byte for clamping
    <<First:8, Middle:240/bitstring, Last:8>> = PrePrivate,

    ReplaceFirst = First band 248,
    ReplaceLast = (Last band 127) bor 64,

    %% return the clamped 32 bytes as a curve 25519 compatible and
    %% hopefully secure key
    <<ReplaceFirst:8, Middle:240/bitstring, ReplaceLast:8>>.

