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
 */

function CSSInjector(cssText) {
    this.contentLoaded = function(event) {
        var styleText = document.createTextNode(this.cssText);

        var styleNode = document.createElement('style');
        styleNode.type = 'text/css';
        styleNode.appendChild (styleText);

        document.head.appendChild(styleNode);
    }

    this.start = function(cssText) {
        this.cssText = cssText;

        if (document.readyState == 'complete')
            this.contentLoaded();
        else
            document.addEventListener('DOMContentLoaded',
                    this.contentLoaded.bind(this) , false);
    }
}
