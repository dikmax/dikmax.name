var https = require('https'),
    fs = require('fs'),
    Q = require('q');

var commentsDir = '../comments/';
var postsDir = '../post';
var disqusApiKey = 'RD99AtEbrzf5EbdWo8ViiTflmX4B7qz7UpXQGjgXASm7uIwwfabTratqcxsZ3FTr';
var defaultSince = '2012-01-01T00:00:00';


var nfapply = function (callback, args) {
    'use strict';

    var deferred = Q.defer();
    args.push(function (err, value) {
        if (err) {
            deferred.reject(err);
        }
        deferred.resolve(value);
    });
    callback.apply(this, args);
    return deferred.promise;
};


var nfcall = function (callback) {
    'use strict';

    var args = Array.prototype.slice.call(arguments, 1);
    return nfapply(callback, args);
};


var sequenceMap = function (fn, arr) {
    'use strict';

    var result = [];
    return arr.reduce(function (promise, item) {
        return promise.then(function () {
            return fn.call(null, item);
        }).then(function (data) {
            result.push(data);
        });
    }, Q())
        .then(function () {
            return result;
        });
};


var request = function (url) {
    'use strict';

    console.log('Request: ' + url + '\n');
    var deferred = Q.defer();
    https.get(url, function (res) {
        res.setEncoding('utf8');
        var response = '';
        res.on('data', function (chunk) {
            response += chunk;
        });
        res.on('end', function () {
            deferred.resolve(JSON.parse(response));
        });
    }).on('error', deferred.reject);

    return deferred.promise;
};


var updateThreadId = function (thread) {
    'use strict';

    return nfcall(fs.readFile, postsDir + '/' + thread.file, {encoding: 'utf-8'})
        .then(function (data) {
            var parts = data.split('---\n');
            var rows = parts[1].split('\n');
            rows[rows.length - 1] = 'thread: ' + thread.thread;
            rows.push('');
            parts[1] = rows.join('\n');
            return nfcall(fs.writeFile, postsDir + '/' + thread.file, parts.join('---\n'), {encoding: 'utf-8'});
        });
};


var specialThreads = [{
    thread: '740092886',
    slug: 'shoutbox'
}, {
    thread: '817355174',
    slug: 'latest'
}];


var getNewComments = function (data) {
    'use strict';

    var threads = data[0].concat(specialThreads);
    var since = data[1];
    var processComment = function (item) {
        var promise;
        var threadsFound = threads.filter(function (thread) {
            return thread.thread === item.thread;
        });
        if (!threadsFound.length) {
            promise = request('https://disqus.com/api/3.0/threads/details.json?thread=' + item.thread +
                    '&api_key=' + disqusApiKey)
                .then(function (response) {
                    var id = response.response.identifiers[0];
                    threadsFound = threads.filter(function (thread) {
                        return thread.slug === id;
                    });
                    if (!threadsFound.length) {
                        throw new Error('Thread ' + item.thread + ' not found.');
                    }
                    var thread = threadsFound[0];
                    thread.thread = response.response.id;
                    return updateThreadId(thread);
                });
        } else {
            promise = Q();
        }
        return promise.then(function () {
            var fileName = item.createdAt + '-' + threadsFound[0].slug + '-' + item.id + '.html';
            var data = '---\n' +
                'id: ' + item.id + '\n' +
                'thread: ' + threadsFound[0].thread + '\n' +
                'date: ' + item.createdAt + '\n' +
                'authorName: ' + item.author.name + '\n' +
                'authorProfile: ' + item.author.profileUrl + '\n' +
                'authorAvatar: ' + item.author.avatar.permalink + '\n' +
                '---\n\n' +
                item.message + '\n';

            return nfcall(fs.writeFile, commentsDir + fileName, data);
        }).catch(function (e) {
            console.log(e.message);
        });
    };

    return request('https://disqus.com/api/3.0/forums/listPosts.json?forum=dikmax&api_key=' + disqusApiKey +
            '&order=asc&limit=100&since=' + since)
        .then(function (response) {
            return sequenceMap(processComment, response.response);
        });
};


var readPostFile = function (fileName) {
    'use strict';

    // TODO subdirectories support
    return nfcall(fs.readFile, postsDir + '/' + fileName, {encoding: 'utf-8'})
        .then(function (data) {
            var rows = data.split('---\n')[1].split('\n');
            var thread = null;
            rows.forEach(function (row) {
                var item = row.split(':');
                if (item[0].trim() === 'thread') {
                    thread = item[1].trim();
                    return false;
                }
                return true;
            });
            return {
                file: fileName,
                slug: fileName.replace(/^\d{4}-\d{2}-\d{2}-(.*)\.md$/, '$1'),
                thread: thread
            };
        });
};


var readSince = function (fileName) {
    'use strict';

    return nfcall(fs.readFile, commentsDir + fileName, {encoding: 'utf-8'})
        .then(function (data) {
            var rows = data.split('---\n')[1].split('\n');
            var since = null;
            rows.forEach(function (row) {
                var item = row.split(':');
                if (item[0].trim() === 'date') {
                    item.shift();
                    since = item.join(':').trim().replace(/^"(.*)"$/, '$1');
                    return false;
                }
                return true;
            });
            return since;
        });
};


var getSince = function () {
    'use strict';

    return nfcall(fs.readdir, commentsDir)
        .then(function (files) {
            files = files.filter(function (name) {
                return name.match(/\.html$/);
            });
            return sequenceMap(readSince, files);
        })
        .then(function (sinces) {
            if (!sinces.length) {
                return defaultSince;
            }
            // Finding max date
            var since = sinces[0];
            for (var i = 1; i < sinces.length; ++i) {
                if (sinces[i] > since) {
                    since = sinces[i];
                }
            }
            return since;
        });
};


var process = function () {
    'use strict';
    Q.all([
        nfcall(fs.readdir, postsDir)
            .then(function (files) {
                return sequenceMap(readPostFile, files);
            }),
        getSince()
    ])
        .then(getNewComments)
        .catch(function (e) {
            console.log(e.stack);
        });
};

process();
