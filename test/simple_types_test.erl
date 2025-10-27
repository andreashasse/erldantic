-module(simple_types_test).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erldantic.hrl").
-include("../include/erldantic_internal.hrl").

-compile(nowarn_unused_type).

-type my_arity() :: arity().
-type my_byte() :: byte().
-type my_char() :: char().
-type my_mfa() :: mfa().
-type my_any() :: any().
-type my_timeout() :: timeout().
-type my_pid() :: pid().
-type my_iodata() :: iodata().
-type my_iolist() :: iolist().
-type my_port() :: port().
-type my_reference() :: reference().
-type my_node() :: node().
-type my_identifier() :: identifier().
-type my_literal() :: 1.
-type my_list() :: list().
-type my_term() :: term().
-type my_nonempty_list() :: nonempty_list().
%% My code formater re-writes nil to [].
-type my_nil() :: [].
-type my_dynamic() :: dynamic().
-type my_nonempty_binary() :: nonempty_binary().
-type my_bitstring() :: bitstring().
-type my_nonempty_bitstring() :: nonempty_bitstring().
-type my_no_return() :: no_return().
-type my_none() :: none().
-type my_range() :: -2..2.

missing_test() ->
    TypeInfo = erldantic_abstract_code:types_in_module(?MODULE),

    %% range
    %% FIXME: more specific matching
    ?assertMatch({error, _}, erldantic_json:to_json(?MODULE, {type, my_range, 0}, -0.0)),

    %% arity
    {ok, MyArityType} = erldantic_type_info:get_type(TypeInfo, my_arity, 0),
    ?assertEqual(
        #ed_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 255
        },
        MyArityType
    ),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_arity, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_arity, 0}, 42)),

    %% byte
    {ok, MyByteType} = erldantic_type_info:get_type(TypeInfo, my_byte, 0),
    ?assertEqual(
        #ed_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 255
        },
        MyByteType
    ),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_byte, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_byte, 0}, 42)),

    %% char
    {ok, MyCharType} = erldantic_type_info:get_type(TypeInfo, my_char, 0),
    ?assertEqual(
        #ed_range{
            type = integer,
            lower_bound = 0,
            upper_bound = 1114111
        },
        MyCharType
    ),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_char, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_char, 0}, 42)),

    %% mfa
    {ok, MyMfaType} = erldantic_type_info:get_type(TypeInfo, my_mfa, 0),
    ?assertEqual(
        #ed_tuple{
            fields =
                [
                    #ed_simple_type{type = atom},
                    #ed_simple_type{type = atom},
                    #ed_range{
                        type = integer,
                        lower_bound = 0,
                        upper_bound = 255
                    }
                ]
        },
        MyMfaType
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_mfa, 0}, {module, function, 42})
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_mfa, 0}, {module, function, 42})
    ),

    %% any
    {ok, MyTermType} = erldantic_type_info:get_type(TypeInfo, my_term, 0),
    ?assertEqual(#ed_simple_type{type = term}, MyTermType),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_term, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_term, 0}, 42)),

    %% timeout
    {ok, MyTimeoutType} = erldantic_type_info:get_type(TypeInfo, my_timeout, 0),
    ?assertEqual(
        #ed_union{
            types =
                [
                    #ed_simple_type{type = non_neg_integer},
                    #ed_literal{value = infinity}
                ]
        },
        MyTimeoutType
    ),
    ?assertMatch(
        {error, [#ed_error{type = no_match}]},
        erldantic_json:to_json(?MODULE, {type, my_timeout, 0}, <<"infinity">>)
    ),
    ?assertEqual(
        {ok, infinity},
        erldantic_json:to_json(?MODULE, {type, my_timeout, 0}, infinity)
    ),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_timeout, 0}, 42)),
    ?assertEqual(
        {ok, infinity},
        erldantic_json:from_json(?MODULE, {type, my_timeout, 0}, infinity)
    ),
    ?assertEqual({ok, 1000}, erldantic_json:from_json(?MODULE, {type, my_timeout, 0}, 1000)),

    %% pid
    {ok, MyPidType} = erldantic_type_info:get_type(TypeInfo, my_pid, 0),
    ?assertEqual(#ed_simple_type{type = pid}, MyPidType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_pid, 0}, self())
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_pid, 0}, <<"not_a_pid">>)
    ),

    %% iodata
    IoList1 = [<<"hello">>, <<"world">>],
    IoList2 = [<<"hello">> | <<"world">>],
    IoList3 = [104, <<"ello">>, [<<"wo">>, 114 | <<"l">>] | <<"d">>],

    {ok, MyIodataType} = erldantic_type_info:get_type(TypeInfo, my_iodata, 0),
    ?assertEqual(#ed_simple_type{type = iodata}, MyIodataType),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iodata, 0}, IoList1)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iodata, 0}, IoList2)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iodata, 0}, IoList3)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iodata, 0}, <<"helloworld">>)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:from_json(?MODULE, {type, my_iodata, 0}, <<"helloworld">>)
    ),

    {ok, MyIolistType} = erldantic_type_info:get_type(TypeInfo, my_iolist, 0),
    ?assertEqual(#ed_simple_type{type = iolist}, MyIolistType),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iolist, 0}, IoList1)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iolist, 0}, IoList2)
    ),
    ?assertEqual(
        {ok, <<"helloworld">>},
        erldantic_json:to_json(?MODULE, {type, my_iolist, 0}, IoList3)
    ),
    ?assertMatch(
        {error, [#ed_error{type = type_mismatch}]},
        erldantic_json:to_json(?MODULE, {type, my_iolist, 0}, <<"helloworld">>)
    ),
    ?assertEqual(
        {ok, [<<"helloworld">>]},
        erldantic_json:from_json(?MODULE, {type, my_iolist, 0}, <<"helloworld">>)
    ),

    %% port
    {ok, MyPortType} = erldantic_type_info:get_type(TypeInfo, my_port, 0),
    ?assertEqual(#ed_simple_type{type = port}, MyPortType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_port, 0}, not_a_port)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_port, 0}, <<"not_a_port">>)
    ),

    %% reference
    {ok, MyReferenceType} = erldantic_type_info:get_type(TypeInfo, my_reference, 0),
    ?assertEqual(#ed_simple_type{type = reference}, MyReferenceType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_reference, 0}, make_ref())
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_reference, 0}, <<"not_a_reference">>)
    ),

    %% node
    {ok, MyNodeType} = erldantic_type_info:get_type(TypeInfo, my_node, 0),
    ?assertEqual(#ed_simple_type{type = atom}, MyNodeType),
    ?assertEqual(
        {ok, nonode@nohost},
        erldantic_json:to_json(?MODULE, {type, my_node, 0}, nonode@nohost)
    ),
    ?assertEqual(
        {ok, nonode@nohost},
        erldantic_json:from_json(?MODULE, {type, my_node, 0}, <<"nonode@nohost">>)
    ),

    %% identifier
    {ok, MyIdentifierType} = erldantic_type_info:get_type(TypeInfo, my_identifier, 0),
    ?assertEqual(
        #ed_union{
            types =
                [
                    #ed_simple_type{type = pid},
                    #ed_simple_type{type = port},
                    #ed_simple_type{type = reference}
                ]
        },
        MyIdentifierType
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_identifier, 0}, my_identifier)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_identifier, 0}, my_identifier)
    ),

    %% literal
    {ok, MyLiteralType} = erldantic_type_info:get_type(TypeInfo, my_literal, 0),
    ?assertEqual(#ed_literal{value = 1}, MyLiteralType),
    ?assertEqual({ok, 1}, erldantic_json:to_json(?MODULE, {type, my_literal, 0}, 1)),
    ?assertEqual({ok, 1}, erldantic_json:from_json(?MODULE, {type, my_literal, 0}, 1)),

    %% list
    {ok, MyListType} = erldantic_type_info:get_type(TypeInfo, my_list, 0),
    ?assertEqual(#ed_list{type = #ed_simple_type{type = term}}, MyListType),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:to_json(?MODULE, {type, my_list, 0}, [1, 2, 3])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:from_json(?MODULE, {type, my_list, 0}, [1, 2, 3])
    ),

    %% term
    {ok, MyTermType2} = erldantic_type_info:get_type(TypeInfo, my_term, 0),
    ?assertEqual(#ed_simple_type{type = term}, MyTermType2),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_term, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_term, 0}, 42)),

    %% nonempty_list
    {ok, MyNonemptyListType} = erldantic_type_info:get_type(TypeInfo, my_nonempty_list, 0),
    ?assertEqual(#ed_nonempty_list{type = #ed_simple_type{type = term}}, MyNonemptyListType),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:to_json(?MODULE, {type, my_nonempty_list, 0}, [1, 2, 3])
    ),
    ?assertMatch(
        {error, [#ed_error{type = type_mismatch}]},
        erldantic_json:to_json(?MODULE, {type, my_nonempty_list, 0}, [])
    ),
    ?assertEqual(
        {ok, [1, 2, 3]},
        erldantic_json:from_json(?MODULE, {type, my_nonempty_list, 0}, [1, 2, 3])
    ),
    ?assertMatch(
        {error, [#ed_error{type = type_mismatch}]},
        erldantic_json:from_json(?MODULE, {type, my_nonempty_list, 0}, [])
    ),

    %% nil
    {ok, MyNilType} = erldantic_type_info:get_type(TypeInfo, my_nil, 0),
    ?assertEqual(#ed_literal{value = []}, MyNilType),
    ?assertEqual({ok, []}, erldantic_json:to_json(?MODULE, {type, my_nil, 0}, [])),
    ?assertEqual({ok, []}, erldantic_json:from_json(?MODULE, {type, my_nil, 0}, [])),

    %% dynamic
    {ok, MyDynamicType} = erldantic_type_info:get_type(TypeInfo, my_dynamic, 0),
    ?assertEqual(#ed_simple_type{type = term}, MyDynamicType),
    ?assertEqual({ok, 42}, erldantic_json:to_json(?MODULE, {type, my_dynamic, 0}, 42)),
    ?assertEqual({ok, 42}, erldantic_json:from_json(?MODULE, {type, my_dynamic, 0}, 42)),

    %% nonempty_binary
    {ok, MyNonemptyBinaryType} =
        erldantic_type_info:get_type(TypeInfo, my_nonempty_binary, 0),
    ?assertEqual(#ed_simple_type{type = nonempty_binary}, MyNonemptyBinaryType),
    ?assertEqual(
        {ok, <<"hello">>},
        erldantic_json:to_json(?MODULE, {type, my_nonempty_binary, 0}, <<"hello">>)
    ),
    ?assertMatch(
        {error, [#ed_error{type = type_mismatch}]},
        erldantic_json:to_json(?MODULE, {type, my_nonempty_binary, 0}, <<>>)
    ),
    ?assertEqual(
        {ok, <<"hello">>},
        erldantic_json:from_json(?MODULE, {type, my_nonempty_binary, 0}, <<"hello">>)
    ),
    ?assertMatch(
        {error, [#ed_error{type = type_mismatch}]},
        erldantic_json:from_json(?MODULE, {type, my_nonempty_binary, 0}, <<>>)
    ),

    %% bitstring
    {ok, MyBitstringType} = erldantic_type_info:get_type(TypeInfo, my_bitstring, 0),
    ?assertEqual(#ed_simple_type{type = bitstring}, MyBitstringType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_bitstring, 0}, <<1, 2, 3>>)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_bitstring, 0}, <<1, 2, 3>>)
    ),

    %% nonempty_bitstring
    {ok, MyNonemptyBitstringType} =
        erldantic_type_info:get_type(TypeInfo, my_nonempty_bitstring, 0),
    ?assertEqual(#ed_simple_type{type = nonempty_bitstring}, MyNonemptyBitstringType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_nonempty_bitstring, 0}, <<1, 2, 3>>)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_nonempty_bitstring, 0}, <<1, 2, 3>>)
    ),

    %% no_return
    {ok, MyNoReturnType} = erldantic_type_info:get_type(TypeInfo, my_no_return, 0),
    ?assertEqual(#ed_simple_type{type = none}, MyNoReturnType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_no_return, 0}, a)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_no_return, 0}, <<"not_a_no_return">>)
    ),

    %% none
    {ok, MyNoneType} = erldantic_type_info:get_type(TypeInfo, my_none, 0),
    ?assertEqual(#ed_simple_type{type = none}, MyNoneType),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:to_json(?MODULE, {type, my_none, 0}, a)
    ),
    ?assertError(
        {type_not_supported, _},
        erldantic_json:from_json(?MODULE, {type, my_none, 0}, <<"not_a_none">>)
    ).
