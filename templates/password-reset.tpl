Dear <editor />,

On March 29th 2013 we discovered that one of the MusicBrainz database dumps
contained password hashes for a large portion of MusicBrainz accounts. While we
don't believe that these password hashes are either useful or widely
distributed, we are requiring all users change their passwords.

The database dumps that contain this data were promptly deleted, and have been
replaced with correctly sanitized database dumps. Unfortunately logs from this
server do show that this database dump was downloaded, and as we have no real
indication of where this data now is, we're treating this seriously. We have
adjusted our database dumping scripts to be very specific about exactly which
data they should export, so that in the future we will not leak private data by
making the same mistake again.

We're extremely sorry about this mistake, and while we don't believe this data
should allow attackers to retrieve user passwords, we can't be 100% certain. As
such, we require that all users change their password as soon as possible.

The next time you login to the website, you will be requested to change your
password. Alternatively, you can go to the following link:

    https://musicbrainz.org/account/change-password?mandatory=1&username=<urlEncode><editor /></urlEncode>

Users should also note that access to authenticated web service calls, for
example to manage tags and ratings via Picard, are also blocked until passwords
are changed. If you are finding that software that uses MusicBrainz is not
behaving as youd expect, please check that you can login via the website.

For more details, please see the blog post:

    http://blog.musicbrainz.org/?p=1844

We're extremely sorry this happened, and thank you for your co-operation.

- The MusicBrainz Team
