teafree
=======

A Haskell utility for tea addicts

Notification
------------

If you are using `xfce4-notifyd`, setting a monospaced font can help readability
of information bubbles. Also a 64x64 image is not awful. Change this by putting
the following lines in your `.gtkrc-2.0` file:

    gtk-icon-sizes = "gtk-dialog=64,64"

    style "notify-font"
    {
        font_name = "Inconsolata"
    }
    widget_class "XfceNotifyWindow.*" style "notify-font"

