\- GNU Emacs 27.1

\- Operating System (Kubuntu):

    Distributor ID: Ubuntu
    Description:    Ubuntu 20.04.1 LTS
    Release:        20.04
    Codename:       focal

I noticed that, when focus stealing preventing is activated, new frames created with `emacsclient` with `emacsclient --alternate-editor="" --no-wait --create-frame`, for example, do not get focus. When I disable [this KDE feature](https://i.imgur.com/aztVHTV.png), the new frame is correctly focused. 



