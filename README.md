# vault

Accessor package for downloading OHDSI goodies :candy:

## How to use

### Set up PAT

First need to set up git credentials via a Personal Access Toekn (PAT). This is best done using the `gitcreds` package. If you have a `GITHUB_PAT` set in your .Renviron, you should reconfigure this to work with the `gitcreds` package as this is the most secure way to handle PATs. To do this, first create a PAT in the developer settings. Once a new PAT is created run:

``` r
gitcreds::gitcreds_set()
```

In the console, paste in your new PAT and set the credentials. You must update this set if your PAT expires. Once this is set you can delete the `GITHUB_PAT` set in your .Renviron. For more thorough instructions follow the documentation of `gitcreds_set`.

### Accessing vaults

After your PAT has been setup using `gitcreds`, you can start working with vault! The first step is to create a vault object. The input for the vault is a repo string structured as follows `<org>/<repo>`. The other input to the vault object is the save path of where to store objects from the vault at checkout. The default save path is the project directory via `here::here()`.

``` r
vv <- vault::vault("OdyOSG/testRepo", savePath = "~/R/my_ohdsi_project")
```

### Checking Vault

Now we have a few options to start with just the vault object initialized. One can `checkAccess` to see if they are able to connect to the organization repository or one can `listContents` of the vault. The `listContents` command returns a tibble with items scrapped from the README.md file of the sub-directory. An example of the `listContents` command would be:

| org    | repo     | name           | type        | version | maintainer      | description                                 |
|-----------|-----------|-----------|-----------|-----------|-----------|-----------|
| OdyOSG | testRepo | simpleCondtion | capr script | 0.0.1   | Martin Lavallee | create a simple condition cohort using Capr |

An example of using these functions is shown below:

``` r
vault::checkAccess(vv)
vault::listContents(vv)
```

### Preview Vault Item

Using the `listContents` tibble, a user can search for items of interest to them. If there is an item of interest, the user can preview the README file in the vault to get more information about the contents. An example of the `preview` command is shown below:

``` r
vault::preview(vv, item = "simpleCondition")
```
where the item 'simpleCondition' is an item in the testRepo directory. The preview command launches the README file in the Viewer tab in RStudio. 



### Checkout Items from vault

Once the user has found the items in the vault we want to use, they can be checkout or downloaded to the path specified in the vault object. The following code shows how to checkout a single item:

``` r
vault::checkout(vv, item = "simpleCondition")
```
This will download the files to your save path and also open the file in RStudio to navigate. The `vault` package also has verbose output, meaning it provides a message in the console detailing what file was downloaded and to where. 

Users can also checkout multiple items from a vault. To do so, the user must specify all items they wish to checkout via a character string. An example is shown below:

```r
cart <- c("simpleCondition", "simpleDrug")
vault::mapCheckout(vv, items = cart)
```


