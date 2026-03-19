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


### Table Joins

Consider the following model:

```lisp
(defparameter *base-types*
  `(:users
     (:base t
       :fields (:name (:type text
                        :ui (:input-type :line :label "Username")
                        :required t
                        :unique t)))

     :resources
     (:base t
       :fields (:name (:type text
                        :ui (:input-type :line :label "Resource")
                        :required t
                        :unique t)
                 :references))

     :permissions
     (:base t
       :fields (:name (:type text
                        :ui (:input-type :line :label "Permission")
                        :required t
                        :unique t)))

     :roles
     (:base t
       :fields (:name (:type text
                        :ui (:input-type :line :label "Roles")
                        :required t
                        :unique t)))

     :role-permissions
     (:base t
       :fields (:reference (:target :roles :lookup-field :name)
                 :reference (:target :permissions :lookup-field :name)))

     :resource-roles
     (:base t
       :fields (:reference (:target :resources :lookup-field :name)
                 :reference (:target :roles :lookup-field :name)))
`
     :role-users
     (:base t
       :fields (:reference (:target :roles :lookup-field :name)
                 :reference (:target :users :lookup-field :name)))

     :widget
     (:fields (:name (:type :text
                       :ui (:input-type :line :label "Widget")
                       :required t
                       :unique t)))))
```

Each of the types listed in the model corresponds to a table in the database. The `:base t` pair indicates that the table already exists, and no SQL needs to be generated to create the table. Thus, the data-ui generates a single table for the above model: the table for `:widget`. This is also the only table who's `id` field (auto-generated) references the `:resources` table. To assign roles to a widget, one need only to manage the resource using the RBAC package. If the widget is deleted, then the system must delete the resource itself. If the resource is deleted, the deletion cascades to the widget. Thus, when deleting a widget, deleting the resource is easier because the database automatically then deletes the widget.

In a hypothetical application that tracks widgets, I might want to list a user, for example, and that listing would entail listing the user's roles. There's a `:users` table, a `:roles` table, and a `:role-users` table (using the terms "table" and "resource type" interchangeably here). The model correctly connects these tables via `:reference` fields, which the compiler converts to the appropriate field names. Consider, for example, the `:role-users` table:

- The first `:reference` field is converted to `:role-id`
- The second `:reference` field is converted to `:user-id`
- The `:lookup-field` value allows us to provide existing options in the UI. In the case of `:role-id`, this becomes a list of the roles from the `:roles` table, where each item in the list comes from the `:name` value of of the role item.

That `:lookup-field` should probably go inside the `:ui` value. But, I need to think of a way to describe what value is rendered for `:role-users`. This might be an attribute of `:roles` and of `:users`. The idea is that the generator has to have a way to determine what to render when displaying a role, for example. The display should include the role name as well as the users associated with that role. I haven't figured out how to organize the model for that.
