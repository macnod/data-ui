## The Resource Type Model

Data UI allows you to create an application by describing it with a Resource Type Model. The resulting application is a complete, database-backed web application with role-based access control (RBAC).

The model is a simple plist that describes resource types. Here's a simple example of a model that describes a To Do list application:

```lisp
'(:users (:base t)
  :todos (:fields (:task (:type :text
                          :input-type :line
                          :required t
                          :unique t
                          :checks-fn (and
                                       (< (length it) 100)
                                       (re:scan "^[-a-zA-Z0-9_ ]+$" it)
                                       (not (re:scan "^[- _]|[- _]$" it))
                                       (not (re:scan "__+|--+|  +" it))))
                   :completed (:type :boolean
                               :input-type :checkbox
                               :default :false)
                   :user (:reference :users
                          :display :name))))
```

The `:users` type has `:base t`, which means it is a pre-existing RBAC table. Data UI will not create a resource type table for `:users`.

Data UI will create a new table for the `:todos` type, because that type is not a base type. Data UI will create the table `rt_todos` for that type, with the specified fields, mapped to SQL columns as follows:

```text
:task       -> todo_task text not null unique
:completed  -> todo_completed boolean not null default false
:references -> user_id uuid not null references users(id) on delete cascade
```

In addition to those fields, the `rt_todos` table, like all resource-type tables, includes these fields by default:

```sql
id uuid not null references resources(id) on delete cascade,
created_at timestamp not null default now(),
updated_at timestamp not null default now()
```

As shown in the SQL above, all resource-type tables reference the RBAC system's resources table. Deleting a row from the resource table cascades to a delete of the corresponding row in a resource-type table.

The code in `:checks-fn` is executed against `it` (the value for the field `:task`) whenever a new resource of type `:todos` is added. The code must be a single form that returns a single value. If the value is `NIL`, the check fails, the resource addition fails, and the record is not added to the table. Otherwise, the resoource addition proceeds as expected. The single-form restriction can be easily overcome with a `progn` block.

The `:options-fn` value also consists of code. You can use this option when you want the front end to provide the user a specific list of values to choose from for this field. The front end can use the values tha tthis code generates to populate a list box, for example.
