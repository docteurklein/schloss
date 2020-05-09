begin;

create schema if not exists schloss;

create table if not exists schloss.migration (id bigint primary key, at timestamptz not null default clock_timestamp());

drop function if exists schloss.migrate;
create function schloss.migrate() returns bigint language plpgsql as $_$
declare current_migration bigint;
begin
    select coalesce((select id from schloss.migration order by id desc limit 1), 0) into current_migration;
    raise notice 'current_migration "%"', current_migration;

    case when current_migration < 1 then

        create extension if not exists "uuid-ossp" with schema schloss;

        create table schloss.messages (
            message_id uuid primary key default schloss.uuid_generate_v4(),
            caused_by uuid references schloss.messages (message_id),
            -- correlation_id uuid not null,
            -- aggregate_id uuid not null,
            name text not null,
            topic text not null,
            -- version text not null,
            added_at timestamptz not null default clock_timestamp(),
            payload jsonb not null,
            policy jsonb
        );
        create or replace rule immutable_message as on update to schloss.messages do instead nothing;
        create or replace rule immortal_message as on delete to schloss.messages do instead nothing;

        -- create index messages_aggregate_id on schloss.messages (aggregate_id);
        create index messages_topic on schloss.messages (topic);

        create or replace function schloss.notify_message() returns trigger language plpgsql as $$
        begin
            perform pg_notify('*', new.message_id::text);
            perform pg_notify(new.topic, new.message_id::text);
            return null;
        end;
        $$;
        create trigger on_message_insert after insert on schloss.messages
        for each row execute function schloss.notify_message();

        insert into schloss.migration (id) values (1);
    else null;
    end case;

    select coalesce((select id from schloss.migration order by id desc limit 1), 0) into current_migration;
    return current_migration;
end;
$_$;

select schloss.migrate() as current_migration;

commit;
