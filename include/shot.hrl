-ifndef(SHOT_HRL).
-define(SHOT_HRL, 1).

%%% ==================================================================
%%% Shot
%%% ==================================================================

-define(NO_REDIRECT, {autoredirect, false}).
-define(CIPHERS, [[{ciphers, [{rsa, aes_128_cbc, sha}]}]]).
-define(SSLC, {ssl, ?CIPHERS}).
-define(BOUNDARY, "----WebKitFormBoundaryShot").
-define(BAD_ARG, {error, bad_arg}).

-endif.
