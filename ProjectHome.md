An Erlang Port API shared library for Linux

# Documentation #

> Doxygen documentation is available [here](http://epapi.googlecode.com/svn/trunk/package/docs/html/index.html).

# Installation #

This package can be installed through [Launchpad](https://launchpad.net/~jldupont/+archive/jldupont).

## Using the library ##

  * Erlang is required
  * The library depends on libei (comes with the default Erlang distribution)
  * Verify other potential dependencies using _pkg-config_ e.g. pkg-config --libs epapi


# Example using MsgHandler #
```
 PktHandler *ph = new PktHandler();
 MsgHandler *mh = new MsgHandler(ph);

 //Register a message type
 // {echo, {Counter}}
 mh->registerType(1, "echo", "l" );

 //Wait for a message
 Msg *m;
 result = mh->rx(&m);

 //Verify return code
 if (result) {
    //handle error
    printf("ERROR, message: %s", mh->strerror());
    // ...
 }
```
More examples are available in the documentation (see link above).

Furthermore, there is another more flexible interface available (**TermHandler**) in the library. An example usage of this interface can be found in the project [Erlang-DBus](http://erlang-dbus.googlecode.com/).
