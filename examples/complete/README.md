## Walkthrough

### File structure

#### `accounts.conf`
This is the configuration file that is given to `abandon`. To run this example, you can invoke this command:

```
abandon_cli.sh -c accounts.conf
```

`abandon` will parse all the input files specified in the configuration file and generate report files in this
directory.

The format of the config file is a superset of JSON. It has some syntax extensions to make it human-friendly.

#### Input files
`Input files` are text files that contain the transaction entries.

Inside `abandon.conf` you will notice this declaration:
```
inputs += accounts.ledger
```

Intuitively, this specifies that the file `accounts.ledger` is an input file.

Why are we using `+=` instead of just `=`? That is because `inputs` is expected to be a list of files, and
by using `+=` we are appending an element to the list.

We could have also written it as:
```
inputs = [accounts.ledger]
```

If there were more than one input files, we could have written:
```
inputs = [first.ledger, second.ledger]
```
or
```
inputs += first.ledger
inputs += second.ledger
```

The `+=` variation is preferred because it is easy to cut/copy/paste entire
lines and because the statements can be specified in different locations of the config
file.

> *Note:* There is yet another way to specify `input files`.
> and that is to use the `include` directive inside the `.ledger`
> file. This example uses the `include` directive. More about it below.


### `accounts.ledger`

`TODO`
