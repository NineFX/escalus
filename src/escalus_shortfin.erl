%%%------------------------------------------------------------------------------------------------
%%% @author Joey Eckstrom <joey.eckstrom@ninefx.com>
%%% @copyright 2021, NineFX, Inc.
%%% @doc 
%%% @end
%%%------------------------------------------------------------------------------------------------

-module(escalus_shortfin).

-behaviour(escalus_user_db).
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

-behaviour(escalus_server).
-export([pre_story/1, 
         post_story/1, 
         name/0]).

-export([seed_domains/1,
         refresh_db/1]).

%%-----------------------------------------------------------------------------------------------%%
%                          BEGIN escalus_server BEHAVIOUR IMPLEMENTATION                          %
%%-----------------------------------------------------------------------------------------------%%
-spec pre_story(escalus:config()) -> escalus:config().
pre_story(Config) ->
    {ok, _} = application:ensure_all_started(shortfin),
    Config.


-spec post_story(escalus:config()) -> escalus:config().
post_story(Config) -> 
    Config.


-spec name() -> atom().
name() -> 
    shortfin.

%%-----------------------------------------------------------------------------------------------%%
%                           END escalus_server BEHAVIOUR IMPLEMENTATION                           %
%%-----------------------------------------------------------------------------------------------%%

%%-----------------------------------------------------------------------------------------------%%
%                          BEGIN escalus_user_db BEHAVIOUR IMPLEMENTATION                         %
%%-----------------------------------------------------------------------------------------------%%

start(_) ->
    ok.

stop(_) ->
    ok.

-spec create_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
create_users(Config, Users) ->
    db_init_domains(Config),
    lists:foreach(fun({_, Spec}) -> register_user(Config, Spec) end, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
delete_users(Config, Users) ->
    lists:foreach(fun({_, Spec}) -> 
        unregister_user(Config, Spec) 
    end, Users),
    Config.

db_init_domains(Config) ->
    Domains = escalus_config:get_config(shortfin_domains, Config),
    lists:foreach(fun({Domain, Vhosts}) ->
        Id = case shortfin_query:register_domain(Domain) of
                ok ->
                    Id_ = shortfin_query:get_domain_id(Domain),
                    Id_;
                DomId when is_integer(DomId) ->
                    DomId
            end,
        ok = shortfin_query:add_vhosts([[H, Id] || H <- Vhosts])
    end, Domains).

db_clear_domains(Config) ->
    Domains = escalus_config:get_config(shortfin_domains, Config),
    lists:foreach(fun({Domain, _}) ->
        Id = shortfin_query:get_domain_id(Domain),
        shortfin_query:unregister_domain(Id)
    end, Domains).

register_user(Config, Spec) ->
    [User, Server, Pass] = escalus_users:get_usp(Config, Spec),
    _Id = shortfin_query:register_user(User, Server, Pass),
    ok.

unregister_user(Config, Spec) ->
    [User, Server, Pass] = escalus_users:get_usp(Config, Spec),
    ok = shortfin_query:unregister_user(User, Server, Pass),
    ok.

%%-----------------------------------------------------------------------------------------------%%
%                           END escalus_user_db BEHAVIOUR IMPLEMENTATION                          %
%%-----------------------------------------------------------------------------------------------%%

%%-----------------------------------------------------------------------------------------------%%
%                         BEGIN escalus_shortfin PUBLIC API IMPLEMENTATION                        %
%%-----------------------------------------------------------------------------------------------%%

%%-----------------------------------------------------------------------------------------------%%
%                          END escalus_shortfin PUBLIC API IMPLEMENTATION                         %
%%-----------------------------------------------------------------------------------------------%%

-spec seed_domains(escalus:config()) -> ok.
seed_domains(Config) ->
    % start shortfin in order to access db functions
    {ok, _} = application:ensure_all_started(shortfin),
    % init the domains and vhosts defined in config
    db_init_domains(Config),
    %application:takeover(shortfin, permanent),
    ok.
    

-spec refresh_db(escalus:config()) -> ok.
refresh_db(Config) ->
    db_clear_domains(Config).
