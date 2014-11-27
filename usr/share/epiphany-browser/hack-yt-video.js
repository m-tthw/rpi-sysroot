/*
 * Copyright © 2013-2014 Klemens Schölhorn
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
 * Note that part of this file is based on Klemens Schölhorn's Firefox
 * extension, see <https://github.com/klemens/ff-youtube-all-html5/>.
 * The code was relicensed from GPLv3+ to GPLv2+ with Klemens's permission.
 */

function YtVideoReplacer() {
    this.videoId = null;

    this.removeAllChildren = function(element) {
        while (element.hasChildNodes()) {
            element.removeChild(element.firstChild);
        }
    }

    this.getUrlParams = function() {
        var params = {};
        window.location.search.replace(/[?&]+([^=&]+)=([^&]*)/gi,
                function(str, key, value) {
                    params[key] = value;
                });
        return params;
    }

    this.insertVideoIFrame = function(video, insertInto) {
        var player = document.createElement('iframe');
        player.src = location.protocol + '//www.youtube.com/embed/' + video +
            '?rel=0&autplay=0&fs=0';
        player.id = 'fallbackIFrame';
        player.width = '100%';
        player.height = '100%';

        var appendIFrame = function() {
            this.removeAllChildren(insertInto);
            insertInto.appendChild(player);
        }.bind(this);

        function nodeRemoved(event) {
            // The node was removed. Sometimes it happens and I don't
            // understand why or what removes it. We just readd the
            // iframe back.
            window.setTimeout(function() { appendIFrame(); }, 1000);
        }
        player.addEventListener('DOMNodeRemovedFromDocument',
                nodeRemoved, false);

        player.stopReadding = function() {
            // We want to be able to remove the iframe without it adding
            // itself back.
            this.removeEventListener('DOMNodeRemovedFromDocument',
                    nodeRemoved, false);
        };

        appendIFrame();
    }

    this.removeExistingVideoIFrame = function() {
        // When we navigate from one YouTube page to another, the previous
        // content is not unloaded, so the previous iframe still exists.
        var existing = document.getElementById('fallbackIFrame');
        if (existing && existing.stopReadding) {
            existing.stopReadding();
            existing.parentNode.removeChild(existing);
        }
    }

    this.createEmbedVideo = function() {
        var insertInto = document.getElementById('player-api-legacy') ||
                         document.getElementById('player-api');
        if(insertInto) {
            this.removeAllChildren(insertInto);

            // We don't insert the video immediately because sometimes it
            // mysteriously doesn't work and because using a timeout makes
            // the loading smoother.
            window.setTimeout (
                    function() {
                        this.insertVideoIFrame(this.videoId, insertInto);
                    }.bind(this),
                    2000);

            // All done.
            return true;
        }

        // We couldn't replace the video (yet?).
        return false;
    }

    this.preventVideos = function() {
        if (document.isPreventingVideos) {
            // Due to how WebKit loads videos when you navigate internally,
            // this could be already running.
            return;
        }

        document.isPreventingVideos = true;

        function handleMutations(mutations) {
            var videos = document.getElementsByTagName('video');
            if (videos.length == 0)
                return;

            /* Very rarely this happens and then you hear the sound of the video
             * but no video shows up. And you don't know why and you think you
             * are going crazy.
             * Let's just destroy the videos and pretend this didn't happen. */
            console.log ("Weird! We still found " + videos.length +
                    " <video> element(s). We will just destroy it/them.");

            for (var i = 0; i < videos.length; i++)
                videos[i].parentNode.removeChild(videos[i]);
        }

        var observer = new WebKitMutationObserver(handleMutations);
        observer.observe(document.body, { childList: true, subtree: true });
    }

    this.contentLoaded = function(event) {
        this.removeExistingVideoIFrame();

        this.videoId= this.getUrlParams().v;
        // There's no point in attempting anything is the video ID is
        // not even available.
        if (!this.videoId)
            return;

        this.preventVideos();

        if (this.createEmbedVideo()) {
            // The player already exists in the page.
            return;
        }

        var observer = new WebKitMutationObserver(
                function(mutations) {
                    if (this.createEmbedVideo())
                        observer.disconnect();
                }.bind(this));
        observer.observe(document.body, { childList: true, subtree: true });
    }

    this.start = function() {
        if (document.readyState == 'complete')
            this.contentLoaded();
        else
            document.addEventListener('DOMContentLoaded',
                    this.contentLoaded.bind(this) , false);
    }
}

var replacer = new YtVideoReplacer();
replacer.start();
