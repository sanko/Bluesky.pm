package Bluesky 0.01 {
    use v5.40;
    use Carp qw[carp];
    use bytes;
    use feature 'class';
    no warnings 'experimental::class', 'experimental::try';
    use lib '../../At.pm/lib';
    use At;
    use Path::Tiny;
    use HTTP::Tiny;
    use URI;

    class Bluesky {
        field $at;
        field $service : param //= 'https://bsky.social';
        #
        ADJUST { $at = At->new( service => $service ); }
        method login ( $identifier, $password )  { $at->login( $identifier, $password ); }
        method resume( $accessJwt, $refreshJwt ) { $at->resume( $accessJwt, $refreshJwt ); }
        method session ()                        { $at->session; }
        #
        method did() { $at->did }

        # Feeds and content
        method getTimeline(%args)   { $at->get( 'app.bsky.feed.getTimeline'   => \%args ); }
        method getAuthorFeed(%args) { $at->get( 'app.bsky.feed.getAuthorFeed' => \%args ); }
        method getPostThread(%args) { $at->get( 'app.bsky.feed.getPostThread' => \%args ); }

        method getPost($uri) {
            my $res = $at->get( 'app.bsky.feed.getPosts' => { uris => [$uri] } );
            $res ? $res->{posts}[0] // () : $res;
        }

        method getPosts(@uris) {
            my $res = $at->get( 'app.bsky.feed.getPosts' => { uris => \@uris } );
            $res ? $res->{posts} // () : $res;
        }
        method getLikes(%args) { my $res = $at->get( 'app.bsky.feed.getLikes' => \%args ); }
        method getRepostedBy() { }

        method createPost(%args) {

            # TODO:
            #   - video and recordWithMedia embeds
            #
            my %post = (    # these are the required fields which every post must include
                '$type'   => 'app.bsky.feed.post',
                text      => $args{text}      // '',
                createdAt => $args{timestamp} // $at->now    # trailing "Z" is preferred over "+00:00"
            );

            # indicate included languages (optional)
            $post{langs} = [ ( ( builtin::reftype( $args{lang} ) // '' ) eq 'ARRAY' ) ? @{ $args{lang} } : $args{lang} ] if defined $args{lang};

            # parse out mentions and URLs as "facets"
            if ( length $post{text} > 0 ) {
                my @facets = $self->parse_facets( $post{text} );
                $post{facets} = \@facets if @facets;
            }

            # additional tags (up to 8)
            $post{tags} = [ ( builtin::reftype( $args{tags} ) // '' ) eq 'ARRAY' ? @{ $args{tags} } : $args{tags} ] if defined $args{tags};

            # metadata tags on an atproto record, published by the author within the record (up to 10)
            $post{labels} = {
                '$type' => 'com.atproto.label.defs#selfLabels',
                values  => [
                    map { { '$type' => 'com.atproto.label.defs#selfLabel', val => $_ } }
                        ( ( builtin::reftype( $args{labels} ) // '' ) eq 'ARRAY' ? @{ $args{labels} } : $args{labels} )
                ]
                }
                if defined $args{labels};

            #~ com.atproto.label.defs#selfLabels
            # if this is a reply, get references to the parent and root
            $post{reply} = $self->getReplyRefs( $args{reply_to} ) if defined $args{reply_to};

            # embeds
            if ( defined $args{image} ) {
                $post{embed} = $self->uploadImages( ( ( builtin::reftype( $args{image} ) // '' ) eq 'ARRAY' ) ? @{ $args{image} } : $args{image} );
            }
            if ( defined $args{video} ) {
                $post{embed} = $self->uploadVideo( ( ( builtin::reftype( $args{video} ) // '' ) eq 'ARRAY' ) ? @{ $args{video} } : $args{video} );
            }
            elsif ( defined $args{embed_url} ) {
                $post{embed} = $self->fetch_embed_url_card( $args{embed_url} );
            }
            elsif ( defined $args{embed_ref} ) {
                $post{embed} = $self->getEmbedRef( $args{embed_ref} );
            }
            $at->post( 'com.atproto.repo.createRecord' => { repo => $self->did, collection => 'app.bsky.feed.post', record => \%post } );
        }

        method deletePost($at_uri) {
            $at_uri = At::Protocol::URI->new($at_uri) unless builtin::blessed $at_uri;
            $at->post( 'com.atproto.repo.deleteRecord' => { repo => $at_uri->host, collection => 'app.bsky.feed.post', rkey => $at_uri->rkey } );
        }
        method like( $uri, $cid )         { }
        method deleteLike($likeUri)       { }
        method repost( $uri, $cid )       { }
        method deleteRepost($repostUri)   { }
        method uploadBlob( $data, %opts ) { }

        # Social graph
        method getFollows()             { }
        method getFollowers()           { }
        method follow($did)             { }
        method deleteFollow($followUri) { }

        # Actors
        method getProfile()             { }
        method upsertProfile()          { }
        method getProfiles()            { }
        method getSuggestions()         { }
        method searchActors()           { }
        method searchActorsTypeadhead() { }
        method mute($did)               { }
        method unmute($did)             { }
        method muteModList($listUri)    { }
        method unmuteModList($listUri)  { }
        method blockModList($listUri)   { }
        method unblockModList($listUri) { }

        # Notifications
        method listNotifications()        { }
        method countUnreadNotifications() { }
        method updateSeenNotifications()  { }

        # Identity
        method resolveHandle() { }
        method updateHandle()  { }

        # Utils
        method parse_mentions($text) {
            my @spans;
            push @spans, { start => $-[1], handle => $2, end => $+[1] }
                while $text =~ /(?:\A|\W)(@(([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?))/g;
            @spans;
        }

        method parse_urls($text) {
            my @spans;

            # partial/naive URL regex based on: https://stackoverflow.com/a/3809435
            # tweaked to disallow some training punctuation
            push @spans, { start => $-[1], url => $1, end => $+[1] }
                while $text
                =~ /(?:\A|\W)(https?:\/\/(www\.)?[-a-zA-Z0-9\@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9\(\)\@:%_\+.~#?&\/\/=]*[-a-zA-Z0-9@%_\+~#\/\/=])?)/g;
            @spans;
        }

        method parse_tags($text) {
            my @spans;
            push @spans, { start => $-[1], tag => $2, end => $+[1] } while $text =~ /(?:\A|\W)(#+(\w{1, 640}))/g;
            @spans;
        }

        method parse_facets($text) {
            my @facets;
            for my $m ( $self->parse_mentions($text) ) {
                my $res = $at->get( 'com.atproto.identity.resolveHandle', { handle => $m->{handle} } );

                # if handle cannot be resolved, just skip it. Bluesky will display it as plain text
                $res || next;
                push @facets,
                    {
                    index    => { byteStart => $m->{start}, byteEnd => $m->{end} },
                    features => [ { '$type' => 'app.bsky.richtext.facet#mention', did => $res->{did} } ]
                    };
            }
            for my $m ( $self->parse_urls($text) ) {
                push @facets,
                    {
                    index    => { byteStart => $m->{start}, byteEnd => $m->{end} },
                    features => [ { '$type' => 'app.bsky.richtext.facet#link', uri => $m->{url} } ]
                    };
            }
            for my $m ( $self->parse_tags($text) ) {
                push @facets,
                    {
                    index    => { byteStart => $m->{start}, byteEnd => $m->{end} },
                    features => [ { '$type' => 'app.bsky.richtext.facet#tag', tag => $m->{tag} } ]
                    };
            }
            @facets;
        }

        method parse_uri($uri) {
            require At::Protocol::URI;    # Should already be loaded but...
            $uri = At::Protocol::URI->new($uri) unless builtin::blessed $uri;
            { repo => $uri->host, collection => $uri->collection, rkey => $uri->rkey };
        }

        method getReplyRefs($parent_uri) {
            my $res = $at->get( 'com.atproto.repo.getRecord', $self->parse_uri($parent_uri) );
            $res || return;
            my $root = my $parent = $res;
            if ( $parent->{value}{reply} ) {
                $root = $at->get( 'com.atproto.repo.getRecord', $self->parse_uri( $parent->{value}{reply}{root}{uri} ) );
                $res ||= $parent;    # escape hatch
            }
            { root => { uri => $root->{uri}, cid => $root->{cid} }, parent => { uri => $parent->{uri}, cid => $parent->{cid} } };
        }

        method uploadFile( $bytes, $mime_type //= () ) {
            if    ( builtin::blessed $bytes ) { $bytes = $bytes->slurp_raw }
            elsif ( ( $^O eq 'MSWin32' ? $bytes !~ m/[\x00<>:"\/\\|?*]/ : 1 ) && -e $bytes ) {
                $bytes = path($bytes)->slurp_raw;
            }

            # TODO: a non-naive implementation would strip EXIF metadata from JPEG files here by default
            my ( $res, $headers ) = $at->post(
                'com.atproto.repo.uploadBlob' => {
                    headers => {
                        'Content-Type' => (
                            defined $mime_type                                         ? $mime_type :
                                $bytes =~ /^GIF89a/                                    ? 'image/gif' :
                                $bytes =~ /^.{2}JFIF/                                  ? 'image/jpeg' :
                                $bytes =~ /^.{4}PNG\r\n\x1a\n/                         ? 'image/png' :
                                $bytes =~ /^.{8}BM/                                    ? 'image/bmp' :
                                $bytes =~ /^.{4}(II|MM)\x42\x4D/                       ? 'image/tiff' :
                                $bytes =~ /^.{4}8BPS/                                  ? 'image/psd' :
                                $bytes =~ /^data:image\/svg+xml;/                      ? 'image/svg+xml' :
                                $bytes =~ /^.{4}ftypqt /                               ? 'video/quicktime' :
                                $bytes =~ /^.{4}ftyp(isom|mp4[12]?|MSNV|M4[v|a]|f4v)/i ? 'video/mp4' :
                                'application/octet-stream'
                        )
                    },
                    content => $bytes
                }
            );
            $res ? $res->{blob} : $res;
        }

        method uploadImages(@images) {
            my @ret;
            for my $img (@images) {
                my $alt  = '';
                my $mime = ();
                if ( ( builtin::reftype($img) // '' ) eq 'HASH' ) {
                    $alt  = $img->{alt};
                    $mime = $img->{mime} // ();
                    $img  = $img->{image};
                }
                if ( builtin::blessed $img ) {
                    At::Error->new( message => 'image file size too large. 1000000 bytes maximum, got: ' . $img->size )->throw
                        if $img->size > 1000000;
                    $img = $img->slurp_raw;
                }
                elsif ( ( $^O eq 'MSWin32' ? $img !~ m/[\x00<>:"\/\\|?*]/ : 1 ) && -e $img ) {
                    $img = path($img);
                    At::Error->new( message => 'image file size too large. 1000000 bytes maximum, got: ' . $img->size )->throw
                        if $img->size > 1000000;
                    $img = path($img)->slurp_raw;
                }
                else {
                    At::Error->new( message => 'image file size too large. 1000000 bytes maximum, got: ' . length $img )->throw
                        if length $img > 1000000;
                }
                my $blob = $self->uploadFile( $img, $mime );
                $blob || $blob->throw;
                push @ret, { alt => $alt, image => $blob };
            }
            { '$type' => 'app.bsky.embed.images', images => \@ret };
        }

        method uploadVideoCaption( $lang, $caption ) {
            if ( builtin::blessed $caption ) {
                At::Error->new( message => 'caption file size too large. 20000 bytes maximum, got: ' . $caption->size )->throw
                    if $caption->size > 20000;
                $caption = $caption->slurp_raw;
            }
            elsif ( ( $^O eq 'MSWin32' ? $caption !~ m/[\x00<>:"\/\\|?*]/ : 1 ) && -e $caption ) {
                $caption = path($caption);
                At::Error->new( message => 'caption file size too large. 20000 bytes maximum, got: ' . $caption->size )->throw
                    if $caption->size > 20000;
                $caption = path($caption)->slurp_raw;
            }
            else {
                At::Error->new( message => 'cation file size too large. 20000 bytes maximum, got: ' . length $caption )->throw
                    if length $caption > 20000;
            }
            my $blob = $self->uploadFile( $caption, 'text/vtt' );
            $blob || $blob->throw;
            { '$type' => 'app.bsky.embed.video#caption', lang => $lang, file => $blob };
        }

        method uploadVideo($vid) {
            my @ret;
            my ( $alt, $mime, $aspectRatio );
            my @captions;
            if ( ( builtin::reftype($vid) // '' ) eq 'HASH' ) {
                $alt         = $vid->{alt};
                $mime        = $vid->{mime} // ();
                $aspectRatio = $vid->{aspectRatio};
                @captions    = map { { lang => $_, file => $self->uploadFile( $vid->{captions}{$_}, 'text/vtt' ) } } keys %{ $vid->{captions} };
                $vid         = $vid->{video};
            }
            if ( builtin::blessed $vid ) {
                At::Error->new( message => 'video file size too large. 50000000 bytes maximum, got: ' . $vid->size )->throw if $vid->size > 50000000;
                $vid = $vid->slurp_raw;
            }
            elsif ( ( $^O eq 'MSWin32' ? $vid !~ m/[\x00<>:"\/\\|?*]/ : 1 ) && -e $vid ) {
                $vid = path($vid);
                At::Error->new( message => 'video file size too large. 50000000 bytes maximum, got: ' . $vid->size )->throw if $vid->size > 50000000;
                $vid = path($vid)->slurp_raw;
            }
            else {
                At::Error->new( message => 'video file size too large. 50000000 bytes maximum, got: ' . length $vid )->throw
                    if length $vid > 50000000;
            }
            my $blob = $self->uploadFile( $vid, $mime );
            $blob || return $blob->throw;
            {   '$type' => 'app.bsky.embed.video',
                video   => $blob,
                ( @captions            ? ( captions    => \@captions )   : () ), ( defined $alt ? ( alt => $alt ) : () ),
                ( defined $aspectRatio ? ( aspectRatio => $aspectRatio ) : () )
            };
        }

        method getEmbedRef($uri) {
            my $res = $at->get( 'com.atproto.repo.getRecord', $self->parse_uri($uri) );
            $res || return;
            { '$type' => 'app.bsky.embed.record', record => { uri => $res->{uri}, cid => $res->{cid} } };
        }

        method fetch_embed_url_card($url) {
            my %card = ( uri => $url, title => '', description => '' );
            state $http //= HTTP::Tiny->new;
            my $res = $http->get($url);
            if ( $res->{success} ) {
                ( $card{title} )       = $res->{content} =~ m[<title>(.*?)</title>.*</head>]is;
                ( $card{description} ) = ( $res->{content} =~ m[<meta name="description" content="(.*?)".+</meta>.*</head>]is ) // '';
                my ($image) = $res->{content} =~ m[<img.*?src="([^"]*)"[^>]*>(?:</img>)?]isp;
                if ( defined $image ) {
                    if ( $image =~ /^data:/ ) {
                        $card{thumb} = $self->uploadFile($image);
                    }
                    else {
                        $res = $http->get( URI->new_abs( $image, $url ) );
                        $card{thumb} = $res->{success} ? $self->uploadFile( $res->{content}, $res->{headers}{'content-type'} ) : ();
                    }
                }
            }
            { '$type' => 'app.bsky.embed.external', external => \%card };
        }

        #~ method block ($actor) {
        #~ my $profile = $self->actor_getProfile($actor);
        #~ builtin::blessed $profile or return;
        #~ $at->post(
        #~ 'com.atproto.repo.createRecord' => {
        #~ repo       => $at->session->did,
        #~ collection => 'app.bsky.graph.block',
        #~ record     => At::Lexicon::app::bsky::graph::block->new( createdAt => time, subject => $profile->did )
        #~ }
        #~ ) ? $at->get( 'com.atproto.actor.getProfile' => { actor => $actor } ) : ();
        #~ }
        #~ method unblock ($actor) {
        #~ my $profile = $self->actor_getProfile($actor);
        #~ builtin::blessed $profile or return;
        #~ return unless $profile->viewer->blocking;
        #~ my ($rkey) = $profile->viewer->blocking =~ m[app.bsky.graph.block/(.*)$];
        #~ $self->repo_deleteRecord( repo => $at->session->did, collection => 'app.bsky.graph.block', rkey => $rkey ) ? $self->profile($actor) : ();
        #~ }
        #~ method follow ($actor) {
        #~ my $profile = $self->actor_getProfile($actor);
        #~ builtin::blessed $profile or return;
        #~ $self->repo_createRecord(
        #~ repo       => $at->did,
        #~ collection => 'app.bsky.graph.follow',
        #~ record     => At::Lexicon::app::bsky::graph::follow->new( createdAt => time, subject => $profile->did )
        #~ ) ? $self->profile($actor) : ();
        #~ }
        #~ method unfollow ($actor) {
        #~ my $profile = $self->actor_getProfile($actor);
        #~ builtin::blessed $profile or return;
        #~ return unless $profile->viewer->following;
        #~ my ($rkey) = $profile->viewer->following =~ m[app.bsky.graph.follow/(.*)$];
        #~ $self->repo_deleteRecord( repo => $at->did, collection => 'app.bsky.graph.follow', rkey => $rkey ) ?
        #~ $self->actor_getProfile($actor)->_raw :
        #~ ();
        #~ }
        #~ method like ( $uri, $repo //= () ) {
        #~ $repo //= $at->did;
        #~ my $res = $self->feed_getPosts($uri);
        #~ $res->{posts} // return;
        #~ $at->post(
        #~ 'com.atproto.repo.createRecord' => {
        #~ repo       => $repo,
        #~ collection => 'app.bsky.feed.like',
        #~ record     =>
        #~ At::Lexicon::app::bsky::feed::like->new( createdAt => At::_now(), subject => { cid => $res->{posts}[0]->cid, uri => $uri } )
        #~ }
        #~ );
        #~ my $record = $at->post(
        #~ 'com.atproto.repo.createRecord' =>,
        #~ { '$type' => 'app.bsky.feed.like', repo => $repo, collection => 'app.bsky.feed.like', record => {} }
        #~ );
        #~ }
        #~ method unlike ( $rkey, $repo //= () ) {
        #~ if ( $rkey =~ m[app.bsky.feed.post] ) {
        #~ my $res = $self->feed_getPosts($rkey);
        #~ $rkey = $res->{posts}[0]->viewer->like;
        #~ }
        #~ if ( $rkey =~ m[app.bsky.feed.like/(.*)$] ) {
        #~ $rkey = $1;
        #~ }
        #~ $at->post( 'com.atproto.repo.deleteRecord' => { repo => $repo // $at->did, collection => 'app.bsky.feed.like', rkey => $rkey } );
        #~ }
        #~ method repost ( $uri, $repo //= () ) {
        #~ $repo //= $at->did;
        #~ my $res = $self->feed_getPosts($uri);
        #~ $res->{posts} // return;
        #~ $self->repo_createRecord(
        #~ repo       => $repo,
        #~ collection => 'app.bsky.feed.repost',
        #~ record     =>
        #~ At::Lexicon::app::bsky::feed::repost->new( createdAt => At::_now(), subject => { cid => $res->{posts}[0]->cid, uri => $uri } )
        #~ );
        #~ }
        #~ method profile ($actor) {
        #~ my $res = $self->actor_getProfile($actor);
        #~ builtin::blessed $res? $res->_raw : ();
        #~ }
    }
};
1;
__END__

=encoding utf-8

=head1 NAME

Bluesky - Bluesky Client Library in Perl

=head1 SYNOPSIS

    use Bluesky;
    my $bsky = Bluesky->new();
    $bsky->login( 'sanko', '1111-2222-3333-4444' );
    $bsky->block( 'sankor.bsky.social' );
    $bsky->unblock( 'sankor.bsky.social' );
    # To be continued...

=head1 DESCRIPTION

You shouldn't need to know the AT protocol in order to get things done so I'm including this sugary wrapper so that
L<At> can remain mostly technical.

=head1 Methods

Bluesky.pm is my attempt to make use of Perl's class syntax so this is obviously OO.

=head2 C<new( ... )>

    my $bsky = Bluesky->new( 'sanko', '1111-2222-3333-4444' );

Expected parameters include:

=over

=item C<identifier> - required

Handle or other identifier supported by the server for the authenticating user.

=item C<password> - required

This is the app password not the account's password. App passwords are generated at
L<https://bsky.app/settings/app-passwords>.

=back

=head2 C<getTimeline( [...] )>

    $bsky->getTimeline();

Get a view of the requesting account's home timeline. This is expected to be some form of reverse-chronological feed.

Expected parameters include:

=over

=item C<algorithm>

Variant 'algorithm' for timeline. Implementation-specific.

NOTE: most feed flexibility has been moved to feed generator mechanism.

=item C<limit>

Integer.

Default: C<50>, Minimum: C<1>, Maximum: C<100>.

=item C<cursor>

=back

=head2 C<getAuthorFeed( ... )>

    $bsky->getAuthorFeed( actor => 'sankor.bsky.social' );

Get a view of an actor's 'author feed' (post and reposts by the author).

Expected parameters include:

=over

=item C<actor> - required

AT-identifier for the author.

=item C<limit>

Integer.

Default: C<50>, Minimum: C<1>, Maximum: C<100>.

=item C<cursor>

=item C<filter>

Combinations of post/repost types to include in response.

Known values:

=over

=item C<posts_with_replies> - default

=item C<posts_no_replies>

=item C<posts_with_media>

=item C<posts_and_author_threads>

=back

=item C<includePins>

Boolean value (false is default).

=back

An error is returned if the client is blocked by the actor.

=head2 C<getPostThread( ... )>

    $bsky->getPostThread( uri => 'at://bsky.app/app.bsky.feed.post/3l6oveex3ii2l' );

Get posts in a thread. Does not require auth, but additional metadata and filtering will be applied for authed
requests.

Expected parameters include:

=over

=item C<uri> - required

Reference (AT-URI) to post record.

=item C<depth>

How many levels of reply depth should be included in response.

Default: C<6>, Minimum: C<0>, Maximum: C<1000>.

=item C<parentHeight>

How many levels of parent (and grandparent, etc) post to include.

Default: C<80>, Minimum: C<0>, Maximum: C<1000>.

=back

Returns an error if the thread cannot be found.

=head2 C<getPost( ... )>

    $bsky->getPost('at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3l6oveex3ii2');

Gets a single post view for a specified post (by AT-URI).

Expected parameters include:

=over

=item C<uri> - required

AT-URI.

=back

=head2 C<getPosts( ... )>

    $bsky->getPosts(
        'at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3l6oveex3ii2l',
        'at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3lbvgvbvcf22c'
    );

Gets post views for a specified list of posts (by AT-URI). This is sometimes referred to as 'hydrating' a 'feed
skeleton'.

Expected parameters include:

=over

=item C<uris> - required

List of (up to 25) post AT-URIs to return hydrated views for.

=back

=head2 C<getLikes( ... )>

    $bsky->getLikes( uri => 'at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.post/3l6oveex3ii2l' );

Get like records which reference a subject (by AT-URI and CID).

Expected parameters include:

=over

=item C<uri> - required

AT-URI of the subject (eg, a post record).

=item C<cid>

CID of the subject record (aka, specific version of record), to filter likes.

=item C<limit>

Integer.

Default: 50, Minimum: 1, Maximum: 100.

=item C<cursor>

=back

=head2 C<createPost( ... )>

    $bsky->createPost( text => 'Test. Test. Test.' );

    $bsky->createPost(
        reply_to   => 'at://did:plc:pwqewimhd3rxc4hg6ztwrcyj/app.bsky.feed.post/3lbvllq2kul27',
        text       => 'Exactly!'
    );

    $bsky->createPost(
        embed_url => 'https://en.wikipedia.org/wiki/Main_Page',
        text       => <<'END');
    This is the link to wikipedia, @atproto.bsky.social. You should check it out.
    END

    $bsky->createPost(
        image     => 'path/to/my.jpg',
        lang       => 'en',
        reply_to   => 'at://did:plc:pwqewimhd3rxc4hg6ztwrcyj/app.bsky.feed.post/3lbvllq2kul27',
        text       => 'I found this image on https://google.com/'
    );

    $bsky->createPost(
        lang       => ['en', 'ja'],
        reply_to   => 'at://did:plc:pwqewimhd3rxc4hg6ztwrcyj/app.bsky.feed.post/3lbvllq2kul27',
        text       => 'こんにちは, World!'
    );

    $bsky->createPost(
        video      => 'path/to/cat.mpeg',
        text       => 'Loot at this little guy!'
    );

Create a new post.

Expected parameters include:

=over

=item C<text> - required

The primary post content. May be an empty string, if there are embeds.

Annotations of text (mentions, URLs, hashtags, etc) are automatically parsed. These include:

=over

=item mentions

Facet feature for mention of another account. The text is usually a handle, including a '@' prefix, but the facet
reference is a DID.

    This is an example. Here, I am mentioning @atproto.bsky.social and it links to their profile.

=item links

Facet feature for a URL. The text URL may have been simplified or truncated, but the facet reference should be a
complete URL.

    This is an example that would link to Google here: https://google.com/.

=item tags

Facet feature for a hashtag. The text usually includes a '#' prefix, but the facet reference should not (except in the
case of 'double hash tags').

    This is an example that would link to a few hashtags. #perl #atproto

=back

=item C<timestamp>

Client-declared timestamp (ISO 8601 in UTC) when this post was originally created.

Defaults to the current time.

=item C<lang>

Indicates human language of post primary text content.

This is expected to be a comma separated string of language codes (e.g. C<en-US,en;q=0.9,fr>).

Bluesky recommends sending the C<Accept-Language> header to get posts in the user's preferred language. See
L<https://www.w3.org/International/questions/qa-lang-priorities.en> and
L<https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry>.

=item C<reply_to>

AT-URL of a post to reply to.

=item C<image>

One of more images (path name or raw data).

Set alt text by passing a hash:

    [ ..., { alt => 'A person standing outdoors.', image => 'camping.jpg' } ]

=item C<embed_url>

A URL. A card (including the URL, the page title, and a description) will be presented in a GUI.

=item C<embed_ref>

An AT-URL to link from this post.

=item C<labels>

Self-label values for this post. Effectively content warnings.

=item C<tags>

Additional hashtags, in addition to any included in post text and facets.

These are not visible in the current Bluesky interface but do cause posts to return as results to to search (such as
L<https://bsky.app/hashtag/perl>.

=item C<video>

A video to be embedded in a Bluesky record (eg, a post).

This might be a single path, raw data, or a hash reference (if you're really into what and how the video is presented).

If passed a hash, the following are expected:

=over

=item C<video> - required

The path name.

=item C<alt>

Alt text description of the video, for accessibility.

=item C<mime>

Mime type.

We try to figure this out internally if undefined.

=item C<aspectRatio>

Represents an aspect ratio.

It may be approximate, and may not correspond to absolute dimensions in any given unit.

    ...
    aspectRatio =>{ width => 100, height => 120 },
    ...

=item C<captions>

This is a hash reference of up to 20 L<WebVTT|https://en.wikipedia.org/wiki/WebVTT> files organized by language.

    ...
    captions => {
        en => 'english.vtt',
        ja => 'japanese.vtt'
    },
    ...

=back

=back

Note that a post may only contain one of the following embeds: C<image>, C<video>, C<embed_url>, or C<embed_ref>.

=head2 C<deletePost( ... )>

    $bsky->deletePost( 'at://did:plc:pwqewimhd3rxc4hg6ztwrcyj/app.bsky.feed.post/3lcdwvquo7y25' );

    my $post = $bsky->createPost( ... );
    ...
    $bsky->deletePost( $post->{uri} );

Delete a post or ensures it doesn't exist.

Expected parameters include:

=over

=item C<uri> - required

=back































































=begin :future

=head2 C<block( ... )>

    $bsky->block( 'sankor.bsky.social' );

Blocks a user.

Expected parameters include:

=over

=item C<identifier> - required

Handle or DID of the person you'd like to block.

=back

Returns a true value on success.

=head2 C<unblock( ... )>

    $bsky->unblock( 'sankor.bsky.social' );

Unblocks a user.

Expected parameters include:

=over

=item C<identifier> - required

Handle or DID of the person you'd like to block.

=back

Returns a true value on success.

=head2 C<follow( ... )>

    $bsky->follow( 'sankor.bsky.social' );

Follow a user.

Expected parameters include:

=over

=item C<identifier> - required

Handle or DID of the person you'd like to follow.

=back

Returns a true value on success.

=head2 C<unfollow( ... )>

    $bsky->unfollow( 'sankor.bsky.social' );

Unfollows a user.

Expected parameters include:

=over

=item C<identifier> - required

Handle or DID of the person you'd like to unfollow.

=back

Returns a true value on success.

=head2 C<profile( ... )>

    $bsky->profile( 'sankor.bsky.social' );

Gathers profile data.

Expected parameters include:

=over

=item C<identifier> - required

Handle or DID of the person you'd like information on.

=back

Returns a hash of data on success.

=end :future

=head1 See Also

L<App::bsky> - Bluesky client on the command line

=head1 LICENSE

Copyright (C) Sanko Robinson.

This library is free software; you can redistribute it and/or modify it under the terms found in the Artistic License
2. Other copyrights, terms, and conditions may apply to data transmitted through this module.

=head1 AUTHOR

Sanko Robinson E<lt>sanko@cpan.orgE<gt>

=begin stopwords

Bluesky unfollow reposts auth authed

=end stopwords

=cut
