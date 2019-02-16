begin;

drop schema if exists DevUsername cascade;
drop owned by DevUsername;
drop user if exists DevUsername;

create user DevUsername with password 'DevPassword';
create schema authorization DevUsername;

grant all privileges on                  database postgres    to DevUsername;
grant all privileges on                  schema   DevUsername to DevUsername;
grant all privileges on all tables    in schema   DevUsername to DevUsername;
grant all privileges on all sequences in schema   DevUsername to DevUsername;

commit;
