insert into schloss.messages
(name, payload, topic) select
'user_registered' , json_build_object('name' , 'joe-' || i)  , 'users'
from generate_series(1, 1) i;

insert into schloss.messages
(name, payload, topic, caused_by) select
'user_changed_password' , json_build_object('new_hash' , random())  , 'users', message_id
from schloss.messages
where name = 'user_registered';

insert into schloss.messages
(name, payload, topic, caused_by) select
'user_bought_product' , json_build_object('product_id' , random())  , 'users', message_id
from schloss.messages
where name = 'user_registered';

insert into schloss.messages
(name, payload, topic, caused_by) select
'user_paid' , json_build_object('amount' , random())  , 'users', message_id
from schloss.messages
where name = 'user_bought_product';

insert into schloss.messages
(name, payload, topic, caused_by) select
'user_received_product' , json_build_object('amount' , random())  , 'users', message_id
from schloss.messages
where name = 'user_paid';

insert into schloss.messages
(name, payload, topic, caused_by) select
'user_sent_feedback' , json_build_object('amount' , random())  , 'users', message_id
from schloss.messages
where name = 'user_received_product';
