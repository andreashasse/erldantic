-module(dummy_json_products_test).

-export([products_from_json/0]).

-type products() :: [product()].

-type category() :: beauty | fragrances | groceries | furniture.

-record(dimensions, {
    width :: float(),
    height :: float(),
    depth :: float()
}).

-record(review, {
    rating :: 1..5,
    comment :: binary(),
    date :: binary(),
    reviewerName :: binary(),
    reviewerEmail :: binary()
}).

-record(product, {
    id :: integer(),
    title :: binary(),
    category :: category(),
    price :: float(),
    discountPercentage :: number(),
    rating :: float(), %%: 2.56,
    stock :: integer(), %%: 99,
    tags :: [binary()], %%: ["beauty", "mascara"],
    brand :: binary() | undefined, %%: "Essence",
    sku :: binary(), %%: "BEA-ESS-ESS-001",
    weight :: integer(), %%: 4,
    dimensions :: #dimensions{}, %%: { "width": 15.14, "height": 13.08, "depth": 22.99 },
    warrantyInformation :: binary(), %%: "1 week warranty",
    shippingInformation :: binary(), %%: "Ships in 3-5 business days",
    availabilityStatus :: binary(), %%: "In Stock",
    reviews :: [#review{}], %%: [
    returnPolicy :: binary(), %%: "No return policy",
    minimumOrderQuantity :: integer(), %%: 48,
    meta :: #{binary() => binary()},
    images :: [binary()],
    thumbnail :: binary()
}).

-type product() :: #product{}.

-export_type([products/0]).

products_from_json() ->
    {ok, Bin} = file:read_file("test/dummy_json_products.json"),
    {JsonTime, #{<<"products">> := Products}} = timer:tc(fun() -> json:decode(Bin) end),
    eflame:apply(erldantic_json, type_from_json, [?MODULE, products, [Products]]),
    {ErlDaTime, _Res} = timer:tc(fun() -> erldantic_json:type_from_json(?MODULE, products, Products) end),
    io:format("JSON decoding time: ~p µs\n", [JsonTime]),
    io:format("ErlDa decoding time: ~p µs\n", [ErlDaTime]),
    ok.
