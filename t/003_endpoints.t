use v5.40;
use Test2::V0;
use blib;
use lib 'lib', '../At.pm/lib';
use Bluesky;

# Mock At instance (same pattern as t/001_mock.t)
{

    package Mock::At;
    use feature 'class';
    no warnings 'experimental::class';

    class Mock::At {
        field $did : param = 'did:plc:mock-user';
        field $last_get;
        field $last_post;
        field $last_writes = [];
        method did()  {$did}
        method host() {'https://bsky.social'}

        method get ( $method, $params = {} ) {
            $last_get = { method => $method, params => $params };
            if ( $method eq 'app.bsky.feed.getListFeed' ) {
                return { feed => [ { post => { uri => 'at://did:plc:x/app.bsky.feed.post/1' } } ] };
            }
            if ( $method eq 'app.bsky.graph.getListBlocks' or $method eq 'app.bsky.graph.getListMutes' ) {
                return { lists => [ { uri => 'at://did:plc:x/app.bsky.graph.list/1' } ] };
            }
            if ( $method eq 'app.bsky.graph.getListsWithMembership' ) {
                return { listsWithMembership => [ { list => { uri => 'at://did:plc:x/app.bsky.graph.list/1' } } ] };
            }
            if ( $method eq 'app.bsky.graph.getStarterPacksWithMembership' ) {
                return { starterPacksWithMembership => [ { starterPack => { uri => 'at://did:plc:x/app.bsky.graph.starterpack/1' } } ] };
            }
            if ( $method eq 'app.bsky.graph.getSuggestedFollowsByActor' ) {
                return { suggestions => [ { handle => 'friend.bsky.social' } ] };
            }
            if ( $method eq 'app.bsky.graph.searchStarterPacks' ) {
                return { starterPacks => [ { uri => 'at://did:plc:x/app.bsky.graph.starterpack/1' } ] };
            }
            if ( $method eq 'app.bsky.notification.getPreferences' ) {
                return { preferences => { priority => 1 } };
            }
            if ( $method eq 'app.bsky.notification.putPreferencesV2' ) {
                return { preferences => { chat => { include => 'all' } } };
            }
            if ( $method eq 'app.bsky.notification.listActivitySubscriptions' ) {
                return { subscriptions => [ { handle => 'fan.bsky.social' } ] };
            }
            if ( $method eq 'app.bsky.video.getUploadLimits' ) {
                return { canUpload => 1, remainingDailyVideos => 9 };
            }
            if ( $method eq 'app.bsky.video.getJobStatus' ) {
                return { jobStatus => { jobId => 'job1', state => 'JOB_STATE_COMPLETED' } };
            }
            if ( $method eq 'app.bsky.draft.getDrafts' ) {
                return { drafts => [ { id => 'draft1' } ] };
            }
            if ( $method eq 'app.bsky.contact.getMatches' ) {
                return { matches => [ { handle => 'match.bsky.social' } ] };
            }
            if ( $method eq 'app.bsky.contact.getSyncStatus' ) {
                return { syncStatus => { matchesCount => 2 } };
            }
            if ( $method eq 'app.bsky.contact.importContacts' ) {
                return { matchesAndContactIndexes => [ { contactIndex => 0 } ] };
            }
            if ( $method eq 'app.bsky.contact.verifyPhone' ) {
                return { token => 'single-use-jwt' };
            }
            if ( $method eq 'app.bsky.ageassurance.getConfig' ) {
                return { regions => [] };
            }
            if ( $method eq 'app.bsky.ageassurance.getState' ) {
                return { state => { status => 'unknown', access => 'unknown' }, metadata => {} };
            }
            if ( $method eq 'com.atproto.label.queryLabels' ) {
                return { labels => [ { val => 'porn' } ] };
            }
            return {};
        }

        method post ( $method, $data = {} ) {
            $last_post = { method => $method, data => $data };
            push @$last_writes, { method => $method, data => $data };
            if ( $method eq 'app.bsky.notification.putPreferencesV2' ) {
                return { preferences => { chat => { include => 'all' } } };
            }
            if ( $method eq 'app.bsky.draft.createDraft' ) {
                return { id => 'draft1' };
            }
            if ( $method eq 'app.bsky.contact.verifyPhone' ) {
                return { token => 'single-use-jwt' };
            }
            if ( $method eq 'app.bsky.contact.importContacts' ) {
                return { matchesAndContactIndexes => [ { contactIndex => 0 } ] };
            }
            return { success => 1 };
        }
        method last_get()    {$last_get}
        method last_post()   {$last_post}
        method last_writes() {$last_writes}

        method http() {
            state $h = bless { token_type => 'Bearer' }, 'Mock::HTTP';

            # Add proxy method to mock http
            {
                no warnings 'redefine', 'once';
                *Mock::HTTP::at_protocol_proxy = sub {undef};
            }
            return $h;
        }
    }
}

# We need to override Bluesky's internal _at_for to return our mock
my $mock = Mock::At->new();
{
    no warnings 'redefine';
    *Bluesky::_at_for = sub {$mock};

    # Also override the internal $at field access if possible, or just the accessor
    *Bluesky::at = sub {$mock};
}
my $bsky = Bluesky->new();
subtest 'Feed additions' => sub {
    my $feed = $bsky->getListFeed('at://did:plc:x/app.bsky.graph.list/1');
    is ref $feed,                       'ARRAY',                                'getListFeed returns arrayref';
    is $mock->last_get->{method},       'app.bsky.feed.getListFeed',            'getListFeed routes correctly';
    is $mock->last_get->{params}{list}, 'at://did:plc:x/app.bsky.graph.list/1', 'getListFeed passes list';
    $bsky->sendInteractions( interactions => [ { item => 'at://did:plc:x/app.bsky.feed.post/1', event => 'app.bsky.feed.defs#interactionSeen' } ] );
    is $mock->last_post->{method}, 'app.bsky.feed.sendInteractions', 'sendInteractions routes correctly';
};
subtest 'Graph additions' => sub {
    is ref $bsky->getListBlocks(),                                     'ARRAY',              'getListBlocks returns arrayref';
    is ref $bsky->getListMutes(),                                      'ARRAY',              'getListMutes returns arrayref';
    is ref $bsky->getListsWithMembership('target.bsky.social'),        'ARRAY',              'getListsWithMembership returns arrayref';
    is $mock->last_get->{params}{actor},                               'target.bsky.social', 'getListsWithMembership passes actor';
    is ref $bsky->getStarterPacksWithMembership('target.bsky.social'), 'ARRAY',              'getStarterPacksWithMembership returns arrayref';
    my $sug = $bsky->getSuggestedFollowsByActor('bsky.app');
    is $sug->[0]{handle},                            'friend.bsky.social', 'getSuggestedFollowsByActor unwraps suggestions';
    is ref $bsky->searchStarterPacks( q => 'perl' ), 'ARRAY',              'searchStarterPacks returns arrayref';
};
subtest 'Notification additions' => sub {
    ok $bsky->getNotificationPreferences(), 'getNotificationPreferences returns preferences';
    $bsky->putNotificationPreferences( priority => 1 );
    is $mock->last_post->{method}, 'app.bsky.notification.putPreferences', 'putNotificationPreferences routes correctly';
    ok $bsky->putNotificationPreferencesV2( chat => { include => 'all', push => 1 } ), 'putNotificationPreferencesV2 returns preferences';
    is ref $bsky->listActivitySubscriptions(), 'ARRAY', 'listActivitySubscriptions returns arrayref';
    $bsky->putActivitySubscription( 'did:plc:x', { post => 1, reply => 1 } );
    is $mock->last_post->{method}, 'app.bsky.notification.putActivitySubscription', 'putActivitySubscription routes correctly';
    $bsky->registerPush( serviceDid => 'did:web:x', token => 't', platform => 'ios', appId => 'a' );
    is $mock->last_post->{method}, 'app.bsky.notification.registerPush', 'registerPush routes correctly';
    $bsky->unregisterPush( serviceDid => 'did:web:x', token => 't', platform => 'ios', appId => 'a' );
    is $mock->last_post->{method}, 'app.bsky.notification.unregisterPush', 'unregisterPush routes correctly';
};
subtest 'Video service' => sub {
    my $limits = $bsky->getVideoUploadLimits();
    ok $limits->{canUpload}, 'getVideoUploadLimits returns limits';
    my $job = $bsky->getVideoJobStatus('job1');
    is $job->{state}, 'JOB_STATE_COMPLETED', 'getVideoJobStatus unwraps jobStatus';
};
subtest Drafts => sub {
    is ref $bsky->getDrafts(), 'ARRAY', 'getDrafts returns arrayref';
    my $created = $bsky->createDraft( { posts => [ { text => 'Unfinished thought...' } ] } );
    is $created->{id}, 'draft1', 'createDraft returns id';
    $bsky->updateDraft( { id => 'draft1', draft => { posts => [] } } );
    is $mock->last_post->{method}, 'app.bsky.draft.updateDraft', 'updateDraft routes correctly';
    $bsky->deleteDraft('draft1');
    is $mock->last_post->{method},   'app.bsky.draft.deleteDraft', 'deleteDraft routes correctly';
    is $mock->last_post->{data}{id}, 'draft1',                     'deleteDraft passes id';
};
subtest Contacts => sub {
    is ref $bsky->getContactMatches(), 'ARRAY', 'getContactMatches returns arrayref';
    ok $bsky->getContactSyncStatus(), 'getContactSyncStatus returns status';
    is ref $bsky->importContacts( token => 'jwt', contacts => ['+12125550123'] ), 'ARRAY', 'importContacts unwraps matches';
    $bsky->dismissContactMatch('did:plc:bad');
    is $mock->last_post->{method}, 'app.bsky.contact.dismissMatch', 'dismissContactMatch routes correctly';
    $bsky->removeContactData();
    is $mock->last_post->{method}, 'app.bsky.contact.removeData', 'removeContactData routes correctly';
    $bsky->startPhoneVerification('+12125550123');
    is $mock->last_post->{method},                     'app.bsky.contact.startPhoneVerification', 'startPhoneVerification routes correctly';
    is $bsky->verifyPhone( '+12125550123', '123456' ), 'single-use-jwt',                          'verifyPhone returns token';
};
subtest 'Age assurance and labels' => sub {
    $bsky->beginAgeAssurance( email => 'me@example.com', language => 'en', countryCode => 'US' );
    is $mock->last_post->{method}, 'app.bsky.ageassurance.begin', 'beginAgeAssurance routes correctly';
    ok $bsky->getAgeAssuranceConfig(),                     'getAgeAssuranceConfig returns config';
    ok $bsky->getAgeAssuranceState( countryCode => 'US' ), 'getAgeAssuranceState returns state';
    my $labels = $bsky->queryLabels( uriPatterns => ['at://did:plc:x/*'] );
    is $labels->[0]{val}, 'porn', 'queryLabels unwraps labels';
};
#
done_testing();
