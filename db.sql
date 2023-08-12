create extension "uuid-ossp";

create table contract (
    id uuid not null,
    name text not null
);

create table entity (
    id uuid not null,
    name text not null,
    age int not null,
    height int not null
);

create table contract_entity (
    contract_id uuid not null,
    entity_id uuid not null
);

insert into contract (id, name) values (
    '1ff0ca77-c13f-4af8-9166-72373f309247',
    'Contract 1 BMW X3 Turbo'
);

insert into entity (id, name, age, height) values (
    'c4958a82-7ba8-498e-8220-a5b10c047229',
    'John',
    20,
    180
), (
    'e787e1b5-4edb-4519-8bd7-8018946c1e2a',
    'Jane',
    30,
    170
);

insert into contract_entity (contract_id, entity_id) values (
    '1ff0ca77-c13f-4af8-9166-72373f309247',
    'c4958a82-7ba8-498e-8220-a5b10c047229'
), (
    '1ff0ca77-c13f-4af8-9166-72373f309247',
    'e787e1b5-4edb-4519-8bd7-8018946c1e2a'
);