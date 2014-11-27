/*
 * Copyright © 2014 Collabora Ltd.
 *     @Author Marco Barisione <marco.barisione@collabora.co.uk>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * NOTE: Parts of this file (clearly marked so) are copied from youtube-dl
 * <https://github.com/rg3/youtube-dl>.
 * youtube-dl is released into the public domain, see <http://unlicense.org/>
 * and so it was relicensed under GPL to fit with the rest of the program.
 */

function parseQueryString(query) {
    if (query[0] == '?')
        query = query.substr(1);

    var params = {};
    var splitQuery = query.split('&');
    for (var i = 0; i < splitQuery.length; i++) {
        var separatorIndex = splitQuery[i].indexOf('=');
        if (separatorIndex == -1)
            continue;
        var key = splitQuery[i].slice(0, separatorIndex);
        var value = splitQuery[i].slice(separatorIndex + 1);
        if (!params[key])
            params[key] = [];
        params[key].push(unescape(value));
    }

    return params;
}

function EmbedLoader(embedUrl, delayLoad) {
    this.embedUrl = embedUrl;

    this.videoId = null; // The YouTube video ID
    this.video = null; // The <video> element
    this.container = null; // The <div> where to insert the video/error
    this.poster = null; // The <img> for the poster
    this.playArea = null; // The play button area
    this.delayLoad = delayLoad; // Whether to load the video now or later
    this.videoLoaded = false; // Whether the video is already loaded

    this.sortFormats = function(formats, maxWidth, maxHeight) {
        var good = [];    // Fit in screen and generally useful.
        var bad = [];     // They don't fit but they are good videos.
        var veryBad = []; // Not very useful.
        var maxSmoothHeight = 720;

        for (var i = 0; i < formats.length; i++) {
            var format = formats[i];
            if (format.height === undefined)
                veryBad.push(format); // Not very useful without height.
            else if (format.preference < 0)
                veryBad.push(format); // Not a very good format according to youtube-dl.
            else if (format.containerFormatPreference < 0)
                veryBad.push(format); // Not a very good format for the Pi.
            else if (format.height > maxSmoothHeight)
                veryBad.push(format); // We cannot play this smoothly.
            else if (format.width === undefined)
                bad.push(format); // No width is still ok as height is what matters the most.
            else if (format.width > maxWidth || format.height > maxHeight)
                bad.push(format); // It doesn't fit, but it's ok to use.
            else
                good.push(format);
        }

        function compareGeneric(a, b, heightComparisonMultiplier) {
            // Bad ones (with lower preference) always go to the back.
            var cmp = b.preference - a.preference;
            if (cmp != 0)
                return cmp;

            // If both have the same preference then we look at height.
            // If heightComparisonMultiplier > 0 then the comparison is ascending.
            // If heightComparisonMultiplier < 0 then the comparison is descending.
            cmp = (a.height || 0) - (b.height || 0);
            if (cmp != 0)
                return cmp * heightComparisonMultiplier;

            // Same size and preference, so let's look at the best format.
            return b.containerFormatPreference - a.containerFormatPreference;
        }

        function compareSmallToBig(a, b) {
            return compareGeneric(a, b, 1);
        }

        function compareBigToSmall(a, b) {
            return compareGeneric(a, b, -1);
        }

        good.sort(compareBigToSmall);
        bad.sort(compareSmallToBig);
        veryBad.sort(compareSmallToBig);

        var sortedFormats = good.concat(bad).concat(veryBad);
        this.debugFormats(sortedFormats);
        return sortedFormats;
    }

    this.debugFormats = function(formats) {
        // Comment this return to debug the format choice.
        return;

        function representSize(size) {
            if (isNaN(size))
                return '???';
            else
                return size.toString();
        }

        for (var i = formats.length - 1; i >= 0; i--) {
            var f = formats[i];
            console.log(
                    "\n" +
                    "\n" +
                    "== Stream #" + i + " ==\n" +
                    "format id: " + f.format_id + "\n" +
                    "extension: " + f.ext + "\n" +
                    "size: " + representSize(f.width) + "x" + representSize(f.height) + "\n" +
                    "format note: " + f.format_note + "\n" +
                    "preference: " + f.preference + "\n" +
                    "url: " + f.url + "\n" +
                    "\n");
        }

        console.log("The stream with format id " + formats[0].format_id +
                " (extension: " + formats[0].ext + ", size: " +
                representSize(formats[0].width) + "x" + representSize(formats[0].height) +
                ") will be played in a " +
                this.container.offsetWidth + "x" + this.container.offsetHeight +
                " container.");
    }

    this.displayError = function (errorMessage) {
        if (!errorMessage || errorMessage == "")
            errorMessage = "unknown error";

        if (this.video)
            this.video.parentNode.removeChild(this.video);

        var videoIdStr;
        if (this.videoId)
            videoIdStr = " (id: " + this.videoId + ") ";
        else
            videoIdStr = " ";

        container.innerHTML =
            "<div id=\"error\">" +
            "<h1>Cannot load the video</h1>" +
            "<p>The video" + videoIdStr + "cannot be loaded: “" + errorMessage + "”.</p>" +
            "</div>";
    }

    this.loadVideo = function() {
        if (this.videoLoaded)
            return;
        this.videoLoaded = true;

        var parsedResults = getFormatsFromId(this.videoId);
        if (parsedResults.success()) {
            formats = this.sortFormats(parsedResults.formats,
                    this.container.offsetWidth, this.container.offsetHeight);
            this.video.setAttribute('src', formats[0].url);
            this.video.setAttribute('controls', '');
        } else {
            this.displayError(parsedResults.error);
        }
    }

    this.getPosterImageFromApi = function(width, height) {
        // This JSON file contains a lot of metadata on the video, including
        // links to the thumbnails.
        var posterData = _download_webpage('http://gdata.youtube.com/feeds/api/videos/' +
                this.videoId + '?v=2&alt=json');
        if (!posterData)
            return;

        var posterJson = JSON.parse(posterData);
        if (!posterJson)
            return;
        var entry = posterJson['entry'];
        if (!entry)
            return;
        var mediaGroup = entry['media$group'];
        if (!mediaGroup)
            return;
        var mediaThumbnail = mediaGroup['media$thumbnail'];
        if (!mediaThumbnail)
            return;

        var smallThumbnails = [];
        var bigThumbnails = [];

        for (var i = 0; i < mediaThumbnail.length; i++) {
            var t = mediaThumbnail[i];

            if (!t.url || !t.width || !t.height)
                continue;

            if (t.width > width || t.height > height)
                bigThumbnails.push(t);
            else
                smallThumbnails.push(t);
        }

        function compareSmallToBig(a, b) {
            return a.height - b.height;
        }

        function compareBigToSmall(a, b) {
            return -compareSmallToBig(a, b);
        }

        smallThumbnails.sort(compareBigToSmall);
        bigThumbnails.sort(compareSmallToBig);
        var sortedThumbnails = bigThumbnails.concat(smallThumbnails);
        if (!sortedThumbnails.length)
            return;

        return sortedThumbnails[0];
    }

    this.getPosterImage = function(width, height) {
        var image = this.getPosterImageFromApi(width, height);
        if (image)
            return image;
        else
            // If something went wrong with just fallback to this image which
            // always exists.
            return { 'url':    'http://i1.ytimg.com/vi/' + this.videoId + '/mqdefault.jpg',
                     'width':  320,
                     'height': 180 };
    }

    this.startVideo = function() {
        this.loadVideo();
        this.video.style.display = 'initial';
        // Setting autoplay at this stage doesn't work.
        window.setTimeout(function() { this.video.play(); }.bind(this), 100);

        this.poster.parentNode.removeChild(this.poster);
        this.playArea.parentNode.removeChild(this.playArea);
    }

    this.createPlayButton = function() {
        var playButton = document.createElement('span');
        playButton.id = 'play-button';

        this.playArea = document.createElement('a');
        this.playArea.id = 'play-area';
        this.playArea.onclick = this.startVideo.bind(this);
        this.playArea.appendChild(playButton);

        this.playArea.style.position = 'absolute';
        this.playArea.style.left = '0';
        this.playArea.style.top = '0';
        this.playArea.style.display = 'block';
        this.playArea.style.width = '100%';
        this.playArea.style.height = '100%';

        container.appendChild(this.playArea);
    }

    this.createPoster = function() {
        var image = this.getPosterImage(this.container.offsetWidth, this.container.offsetHeight);

        // The thumbnail is often in 4:3 even for wide videos and includes
        // top and bottom black bars. This means that, if we just insert the
        // stretched poster as we get it, we end up with bars on all four
        // sides!
        // We stretch the image so that both its width and height are at
        // least as big as the container. Doing so avoids black bars due
        // to the image being too small and should also crop away the black
        // bars contained in the image.
        // Due to rounding sometimes there's one pixel of black bars form
        // the thumbnail still visible. We pretend the container is 2 pixel
        // wider and 2 pixel larger, so that we crop away these leftover
        // black bars.
        var ratioWidth = (this.container.offsetWidth + 2) / image.width;
        var ratioHeight = (this.container.offsetHeight + 2) / image.height;
        var ratio = Math.max(ratioWidth, ratioHeight);
        var imageDisplayWidth = image.width * ratio;
        var imageDisplayHeight = image.height * ratio;

        this.poster = document.createElement('img');
        this.poster.id = 'poster';
        this.poster.src = image.url;
        this.poster.style.width = imageDisplayWidth + 'px';
        this.poster.style.height = imageDisplayHeight + 'px';
        this.poster.style.position = 'absolute';
        this.poster.style.left = (this.container.offsetWidth - imageDisplayWidth) / 2 + 'px';
        this.poster.style.top = (this.container.offsetHeight - imageDisplayHeight) / 2 + 'px';

        this.container.appendChild(this.poster);
    }

    this.createVideoTag = function() {
        this.video = document.createElement('video');
        this.video.id = 'yt-replacement-video';
        this.video.setAttribute('width', this.container.offsetWidth);
        this.video.setAttribute('height', this.container.offsetHeight);
        // Initially hidden as we have the poster.
        this.video.style.display = 'none';

        this.container.appendChild(this.video);
    }

    this.getVideoIdFromEmbed = function() {
        if (!this.embedUrl)
            return null;

        matchInfo = this.embedUrl.match(/\/embed\/([^?]*)/);
        if (matchInfo)
            return matchInfo[1];
        else
            return null;
    }

    this.insertVideo = function(container) {
        this.container = container;
        this.videoId = this.getVideoIdFromEmbed(this.embedUrl);

        if (this.videoId) {
            this.createVideoTag();
            this.createPoster();
            this.createPlayButton();
            if (!this.delayLoad)
                this.loadVideo();
        } else if (this.embedUrl) {
            this.displayError(this.embedUrl + " is not a valid embed URL");
        } else {
            this.displayError("no video specified");
        }
    }
}


// The following code is based on the youtube-dl program from
// <https://github.com/rg3/youtube-dl> and translated from Python
// to JavaScript. This translation was done in a way to minimise
// unneeded change, so it will be easier to update the code in the
// future. This explains the weird non-idiomatic coding style.
//
// youtube-dl's original code was released into the public domain,
// see <http://unlicense.org/>.
//
// The latest commit in the youtube-dl's version this is based on is:
// commit 29f6ed78e87946979ab6472b118a4da7cf7ef0c0
// Author: Petr Půlpán <Pulpan3@gmail.com>
// Date:   Tue Jul 1 10:35:49 2014 +0200

var _formats = {
    '5': {'ext': 'flv', 'width': 400, 'height': 240},
    '6': {'ext': 'flv', 'width': 450, 'height': 270},
    '13': {'ext': '3gp'},
    '17': {'ext': '3gp', 'width': 176, 'height': 144},
    '18': {'ext': 'mp4', 'width': 640, 'height': 360},
    '22': {'ext': 'mp4', 'width': 1280, 'height': 720},
    '34': {'ext': 'flv', 'width': 640, 'height': 360},
    '35': {'ext': 'flv', 'width': 854, 'height': 480},
    '36': {'ext': '3gp', 'width': 320, 'height': 240},
    '37': {'ext': 'mp4', 'width': 1920, 'height': 1080},
    '38': {'ext': 'mp4', 'width': 4096, 'height': 3072},
    '43': {'ext': 'webm', 'width': 640, 'height': 360},
    '44': {'ext': 'webm', 'width': 854, 'height': 480},
    '45': {'ext': 'webm', 'width': 1280, 'height': 720},
    '46': {'ext': 'webm', 'width': 1920, 'height': 1080},

    // 3d videos
    '82': {'ext': 'mp4', 'height': 360, 'format_note': '3D', 'preference': -20},
    '83': {'ext': 'mp4', 'height': 480, 'format_note': '3D', 'preference': -20},
    '84': {'ext': 'mp4', 'height': 720, 'format_note': '3D', 'preference': -20},
    '85': {'ext': 'mp4', 'height': 1080, 'format_note': '3D', 'preference': -20},
    '100': {'ext': 'webm', 'height': 360, 'format_note': '3D', 'preference': -20},
    '101': {'ext': 'webm', 'height': 480, 'format_note': '3D', 'preference': -20},
    '102': {'ext': 'webm', 'height': 720, 'format_note': '3D', 'preference': -20},

    // Apple HTTP Live Streaming
    '92': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'preference': -10},
    '93': {'ext': 'mp4', 'height': 360, 'format_note': 'HLS', 'preference': -10},
    '94': {'ext': 'mp4', 'height': 480, 'format_note': 'HLS', 'preference': -10},
    '95': {'ext': 'mp4', 'height': 720, 'format_note': 'HLS', 'preference': -10},
    '96': {'ext': 'mp4', 'height': 1080, 'format_note': 'HLS', 'preference': -10},
    '132': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'preference': -10},
    '151': {'ext': 'mp4', 'height': 72, 'format_note': 'HLS', 'preference': -10},

    // DASH mp4 video
    '133': {'ext': 'mp4', 'height': 240, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '134': {'ext': 'mp4', 'height': 360, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '135': {'ext': 'mp4', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '136': {'ext': 'mp4', 'height': 720, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '137': {'ext': 'mp4', 'height': 1080, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '138': {'ext': 'mp4', 'height': 2160, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '160': {'ext': 'mp4', 'height': 144, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '264': {'ext': 'mp4', 'height': 1440, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},

    // Dash mp4 audio
    '139': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 48, 'preference': -50},
    '140': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 128, 'preference': -50},
    '141': {'ext': 'm4a', 'format_note': 'DASH audio', 'vcodec': 'none', 'abr': 256, 'preference': -50},

    // Dash webm
    '167': {'ext': 'webm', 'height': 360, 'width': 640, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '168': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '169': {'ext': 'webm', 'height': 720, 'width': 1280, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '170': {'ext': 'webm', 'height': 1080, 'width': 1920, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '218': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '219': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'acodec': 'none', 'container': 'webm', 'vcodec': 'VP8', 'preference': -40},
    '242': {'ext': 'webm', 'height': 240, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '243': {'ext': 'webm', 'height': 360, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '244': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '245': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '246': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '247': {'ext': 'webm', 'height': 720, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '248': {'ext': 'webm', 'height': 1080, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '271': {'ext': 'webm', 'height': 1440, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},
    '272': {'ext': 'webm', 'height': 2160, 'format_note': 'DASH video', 'acodec': 'none', 'preference': -40},

    // Dash webm audio
    '171': {'ext': 'webm', 'vcodec': 'none', 'format_note': 'DASH audio', 'abr': 48, 'preference': -50},
    '172': {'ext': 'webm', 'vcodec': 'none', 'format_note': 'DASH audio', 'abr': 256, 'preference': -50},

    // RTMP (unnamed)
    '_rtmp': {'protocol': 'rtmp'},
};

// Compatibility with Python
String.prototype.startsWith = function(str) {
    return this.indexOf(str) == 0;
}

function _download_webpage(url){
    try {
        var request = new XMLHttpRequest();
        request.open('GET', url, false );
        request.send('');
        return request.responseText;
    } catch (e) {
        console.log ("Failed to retrieve " + url);
        return '';
    }
}

compat_parse_qs = parseQueryString;

function uppercase_escape(string) {
    // This is used to fix the unicode in descriptions. We ignore description
    // so we just need to get rid of the \U escape sequences that would break
    // the JSON parser.
    return string.replace(/\\U[0-9a-fA-F]{8}/, '');
}

// This is the main extraction function and is based on YoutubeIE._real_extract
// from youtube_dl/extractor/youtube.py.
function getFormatsFromId(video_id) {
    function build_result(formats, errMsg) {
        return { 'formats': formats,
                 'error':   errMsg,
                 success:   function() { return this.error == null; }};
    }

    function err(errMsg) {
        return build_result([], errMsg);
    }

    function success(formats) {
        if (formats.length == 0)
            // Well, not really a success
            return err("no available formats found");
        else
            return build_result(formats, null);
    }

    proto = 'http';

    // Get video webpage
    url = proto + '://www.youtube.com/watch?v=' + video_id + '%s&gl=US&hl=en&has_verified=1';
    video_webpage = _download_webpage(url);

    // Get video info
    if (video_webpage.match(/player-age-gate-content">/)) {
        // In youtube-dl the program gets the age confirmation page and it
        // needs to get the real page from it. In the browser we reach this
        // point only if the user is already logged in, so no need to bother
        // with age consent support.
        return err("age consent should not be reached at this stage");
    } else {
        age_gate = false;
        els = ['&el=embedded', '&el=detailpage', '&el=vevo', ''];
        for (var i = 0; i < els.length; i++) {
            el_type = els[i];
            video_info_url = proto + '://www.youtube.com/get_video_info?&video_id=' +
                video_id + el_type + '&ps=default&eurl=&gl=US&hl=en';
            video_info_webpage = _download_webpage(video_info_url);
            video_info = compat_parse_qs(video_info_webpage);
            if (video_info['token'])
                break;
        }
    }

    if (!video_info['token'])
        return err("cannot find the token");

    // Decide which formats to download
    mobj = video_webpage.match(/;ytplayer\.config\s*=\s*({.*?});/);
    if (mobj) {
        json_code = uppercase_escape(mobj[1]);
        ytplayer_config = JSON.parse(json_code);
        args = ytplayer_config['args'];
        // Easy way to know if the 's' value is in url_encoded_fmt_stream_map
        // this signatures are encrypted
        if (args['url_encoded_fmt_stream_map']) {
            re_signature = /[&,]s=/
            m_s = args['url_encoded_fmt_stream_map'].search(re_signature);
            if (m_s)
                video_info['url_encoded_fmt_stream_map'] = [args['url_encoded_fmt_stream_map']];
            adaptive_fmts = args['adaptive_fmts'];
            if (!adaptive_fmts)
                adaptive_fmts = '';
            m_s = adaptive_fmts.match(re_signature);
            if (m_s) {
                if (video_info['adaptive_fmts'])
                    video_info['adaptive_fmts'][0] += ',' + args['adaptive_fmts'];
                else
                    video_info['adaptive_fmts'] = [args['adaptive_fmts']];
            }
        }
    }

    containerFormatPreferenceMap = {
        'mp4':  10,
        'webm':  9,
        '3gp':  -1,  // Not very good support but they can play
        'flv':  -10, // We cannot accelerate these
        'mpa':  -20, // Audio only
    };
    function _map_to_format_list(urlmap) {
        formats = [];
        for (var itag in urlmap) {
            if (!urlmap.hasOwnProperty(itag))
                continue;
            video_real_url = urlmap[itag];
            dct = {
                'format_id': itag,
                'url': video_real_url,
            };
            var extra_info = _formats[itag];
            if (extra_info) {
                for(var k in extra_info) {
                    if(!extra_info.hasOwnProperty(k))
                        continue;
                    dct[k] = extra_info[k];
                }
            }
            if (dct.preference === undefined)
                dct.preference = 0;
            dct.containerFormatPreference = containerFormatPreferenceMap[dct.ext] || 0;
            formats.push(dct);
        }
        return formats;
    }

    url_encoded_fmt_stream_map = video_info['url_encoded_fmt_stream_map'] || [];
    adaptive_fmts = video_info['adaptive_fmts'] || [];

    if (video_info['conn'] && video_info['conn'][0].startsWith('rtmp')) {
        formats = [{
            'format_id': '_rtmp',
            'protocol': 'rtmp',
            'url': video_info['conn'][0],
        }];
    } else if (url_encoded_fmt_stream_map.length >= 1 || adaptive_fmts.length >= 1) {
        if (!url_encoded_fmt_stream_map.length)
            url_encoded_fmt_stream_map = [''];
        if (!adaptive_fmts.length)
            adaptive_fmts = [''];
        encoded_url_map = url_encoded_fmt_stream_map[0] + ',' + adaptive_fmts[0];
        if (encoded_url_map.indexOf('rtmpe%3Dyes') != -1)
            return err("rtmpe downloads are not supported");
        url_map = {};
        split_encoded_url_map = encoded_url_map.split(',');
        for (var i = 0; i < split_encoded_url_map.length; i++) {
            url_data_str = split_encoded_url_map[i];
            url_data = compat_parse_qs(url_data_str);
            if (url_data['itag'] && url_data['url']) {
                url = url_data['url'][0];
                if (url_data['sig']) {
                    url += '&signature=' + url_data['sig'][0];
                } else if (url_data['s']) {
                    return err("encrypted streams not supported");
                }
                if (url.indexOf('ratebypass') == -1) {
                    url += '&ratebypass=yes';
                }
                url_map[url_data['itag'][0]] = url;
            }
        }
        formats = _map_to_format_list(url_map);
    } else if (video_info['hlsvp']) {
        return err("hlsvp videos are not supported");
    } else {
        return err("no conn, hlsvp or url_encoded_fmt_stream_map information found in video info");
    }

    return success(formats);
}
