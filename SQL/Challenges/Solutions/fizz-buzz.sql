-- * Fizz Buzz et al in PostgreSQL

-- Alas: select array_to_string(ARRAY[]::text[], '')
-- returns '' instead of being NULL.  We're going to
-- work with it instead of defining our own function.

-- ** Table & Select

drop if exists table cnt_says;

create table cnt_says (
  cnt integer not null,
  say text not null
);

insert into cnt_says(cnt, say)
values (3, 'fizz'), (5, 'buzz');

select coalesce(
  nullif(
    array_to_string(
      array(select say from cnt_says where i % cnt = 0 order by cnt),
      ''
    ),
    ''
  ),
  i::text
)
from generate_series(1,20) i;

-- ** Functional Programming

-- This example is inefficient in creating
-- records where only one field is changing.
-- It can be refactored to use two arrays,
-- one with just the cnt values.

-- What a horrendous amount of boilerplate!

drop if exists type cnt_max_say_rows;

create type cnt_max_say_rows as (
  cnt integer,
  max integer,
  say text
);

comment on type cnt_max_say_rows is
'a naive record type for counting and saying';

create or replace function
  cycle_cms(cms cnt_max_say_rows)
returns cnt_max_say_rows AS $$
  select case
    when (cms).cnt = 0
    then row((cms).max - 1, (cms).max, (cms).say)
    else row((cms).cnt - 1, (cms).max, (cms).say)
    end
$$ language sql;

create or replace function
  cycle_cms_array(cms_array cnt_max_say_rows[])
returns cnt_max_say_rows[] AS $$
  select array(select cycle_cms(cms) from unnest(cms_array) cms)
$$ language sql;

create or replace function
  say_cms(cms cnt_max_say_rows)
returns text AS $$
  select case when (cms).cnt = 0 then (cms).say else '' end
$$ language sql;

create or replace function
  say_cms_array(cms_array cnt_max_say_rows[])
returns text AS $$
  select nullif(
    array_to_string(
      array(select say_cms(cms) from unnest(cms_array) cms),
      ''
    ),
  ''
  )
$$ language sql;

create or replace function
  cms_loop(i integer, max integer, cms_array cnt_max_say_rows[])
returns text AS $$
  select case
  when i > max then ''
  else coalesce(say_cms_array(cms_array), i::text) ||
    E'\n' ||
    cms_loop(i+1, max, cycle_cms_array(cms_array))
  end
$$ language sql;

select cms_loop(
  1, 20,
  array[
    cycle_cms(row(3, 3, 'fizz')),
    cycle_cms(row(5, 5, 'buzz'))
  ]
);

-- ** CTE solution by Jeff

with
  range as (select '[1, 20]'::numrange),
  words as (select 3, 'Fizz' union select 5, 'Buzz'),
  _words as (select 1 modulus, '' word union select * from words),
  _aggregated as (
    select index, string_agg(word, '' order by modulus, word) output
    from _words, range, generate_series(lower(numrange), upper(numrange)) index
    where index % modulus = 0 group by 1
  )
  select case when output = '' then index::text else output end
  from _aggregated;
