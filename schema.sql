create schema schloss;

create extension if not exists "uuid-ossp" with schema schloss;

create table schloss.messages (
    message_id uuid primary key default schloss.uuid_generate_v4(),
    caused_by uuid references api.messages (message_id),
    correlation_id uuid not null,
    aggregate_id uuid not null,
    name text not null,
    topic text not null,
    version text not null,
    added_at timestamptz not null default clock_timestamp(),
    payload jsonb not null,
    policy jsonb
);
create or replace rule immutable_message as on update to schloss.message do instead nothing;
create or replace rule immortal_message as on delete to schloss.message do instead nothing;

create index messages_aggregate_id on api.messages (aggregate_id);
create index messages_topic on api.messages (topic);

create or replace function schloss.notify_message() returns trigger language plpgsql as $$
begin
    perform pg_notify(new.topic, new.message_id::text);
    return null;
end;
$$;
create trigger on_message_insert after insert on api.messages
for each row execute function schloss.notify_message();
