teafree
=======

A Haskell utility for tea addicts

Notification
------------

If you are using `xfce4-notifyd`, setting a monospaced font can help readability
of information bubbles. Put the following lines in your `gtkrc` theme:

    style "notify-font"
    {
        font_name = "Inconsolata"
    }
    widget_class "XfceNotifyWindow.*" style "notify-font"

