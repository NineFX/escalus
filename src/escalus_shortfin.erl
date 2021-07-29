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

%%-----------------------------------------------------------------------------------------------%%
%                          BEGIN escalus_server BEHAVIOUR IMPLEMENTATION                          %
%%-----------------------------------------------------------------------------------------------%%
-spec pre_story(escalus:config()) -> escalus:config().
pre_story(Config) ->
    Opts = escalus_config:get_config(epgsql_config, Config),
    {ok, Conn} = epgsql:connect(Opts),
    lists:keystore(epgsql_conn, 1, Config, {epgsql_conn, Conn}).


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
    application:ensure_all_started(epgsql).

stop(_) ->
    ok.

-spec create_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
create_users(Config, Users) ->
    db_init_domains(Config),
    lists:foreach(fun({_, Spec}) -> register_user(Config, Spec) end, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), [escalus_users:named_user()]) -> escalus:config().
delete_users(Config, Users) ->
    lists:foreach(fun({_, Spec}) -> unregister_user(Config, Spec) end, Users),
    Config.

db_init_domains(Config) ->
    Domain = escalus_config:get_config(shortfin_domain, Config),
    Conn = escalus_config:get_config(epgsql_conn, Config),
    {ok, _, _} = epgsql:equery(Conn, "INSERT INTO domains (domain) VALUES ($1) ON CONFLICT DO NOTHING", [Domain]).

hash(Value) ->
    Salt = <<"edde84fd-39e7-431b-b6e8-a9810b33dbe4">>,
    {ok, Key} = pbkdf2:pbkdf2(Value, Salt, 4096, 20),
    Key.

register_user(Config, Spec) ->
    [User, Server, Pass] = escalus_users:get_usp(Config, Spec),
    Conn = escalus_config:get_config(epgsql_conn, Config),
    Hash = hash(Pass),
    {ok, _, _} = epgsql:equery(Conn, "INSERT INTO users (node, domain_id, password)
                                      SELECT $1, domain_id, $3
                                      FROM domains
                                      WHERE domain = $2", [User, Server, Hash]).

unregister_user(Config, Spec) ->
    [User, Server, Pass] = escalus_users:get_usp(Config, Spec),
    Conn = escalus_config:get_config(epgsql_conn, Config),
    Hash = hash(Pass),
    {ok, _, _} = epgsql:equery(Conn, "DELETE FROM users AS u
                                      USING domains AS d
                                      WHERE u.domain_id = d.domain_id AND
                                            u.password=$3 AND
                                            d.domain=$2 AND
                                            u.node=$1 AND", [User, Server, Hash]).

%%-----------------------------------------------------------------------------------------------%%
%                           END escalus_user_db BEHAVIOUR IMPLEMENTATION                          %
%%-----------------------------------------------------------------------------------------------%%
