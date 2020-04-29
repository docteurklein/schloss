with recursive cause (root, message, depth, traversed) as (
    select m.message_id, m, 0, '{}'::uuid[]
    from schloss.messages m
    union all
    select cause.root, m, depth + 1, traversed || m.message_id
    from cause
    join schloss.messages m on (cause.message).caused_by = m.message_id
    where not m.message_id = any(traversed)
)
, chain (root, doc) as (
    select root,
    jsonb_agg(message)
    from cause
    group by root
)
select doc
from chain
where root = :'message_id'
limit 1
;
-- with recursive chain (message_id, caused_by, traversed, doc) as (
--     select m.message_id, m.caused_by, '{}'::uuid[], 0,
--     jsonb_agg(to_jsonb(m) - 'traversed' - 'policy')
--     from schloss.messages m
--     group by message_id
--     union all
--     select cause.message_id, cause.caused_by, traversed || cause.message_id,
--     (to_jsonb(cause) - 'traversed' - 'policy') || jsonb_build_object('caused_by', doc)
--     from chain
--     join schloss.messages cause on chain.message_id = cause.caused_by
--     where not cause.message_id = any(traversed)
-- )
-- select jsonb_agg(doc)
-- from chain
-- where message_id = :'message_id';
-- ;

-- with recursive cause (message_id, traversed, level) as (
--     select m.message_id, '{}'::uuid[], 0
--     from schloss.messages m
--     where m.caused_by is null
--     union all
--     select consequence.message_id, traversed || consequence.caused_by, level + 1
--     from cause
--     join schloss.messages consequence on consequence.caused_by = cause.message_id
--     where not consequence.message_id = any(traversed)
-- )
-- , leaf_consequence (caused_by, graph) as (
--     select m.caused_by,
--     jsonb_agg(to_jsonb(cause) - 'traversed' - 'policy')
--     from cause
--     join schloss.messages m using(message_id)
--     where level > 0 and not message_id = any(traversed)
--     group by m.caused_by
--     union all
--     select m.caused_by,
--     (to_jsonb(m) - 'traversed' - 'policy')
--     || jsonb_build_object('caused', graph)
--     from leaf_consequence
--     join schloss.messages m on m.message_id = leaf_consequence.caused_by
-- )
-- select jsonb_agg(graph)
-- from leaf_consequence
-- where caused_by is null;
