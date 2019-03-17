%%-*- mode: erlang -*-
%%-*- coding: utf-8 -*-

% Test control options

[{tests, []}].

%%
%% TESTS
%%

% ==============================================================================

"
Begin
    Insert Into :par_64__\"@db_link_special$\" I1IDENT_8 (LONG)
        Select /*+ NO_INDEX_FFS(items item_order_ix) */
               Distinct CASEx (CONTENTS => JSON,
                              BAG => :par_2 ) >= LONG.\"ident(s)\".NO (:PAR_62 :PAR_62,:PAR_64 Indicator :par_5_2_)
          From KEY I1IDENT_1_,:par_table_3__ S@N~$_ident
      Order By ABS (All t12_ident|:_a1:f()|) Desc,
               ROLLBACK.\"ident(s)\"|::a| Desc
  Returning p1hone5_ident_scalar_exp != PARTITION.credit_limit_ident_~4.* Into :par_64__ Indicator :par_5_1 I@IDENT_100_@;
End;
".
