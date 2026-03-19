After a model is compiled, data-ui will make RESTful endpoints available for CRUD operations with the specified resources.

## Create Resource

### Endpoint
```text
POST /api/resources/:type
```

### Example
```text
POST /api/resources/:message

{
    :title "First Post"
    :content "She drew a circle that shut me out"
}
```
