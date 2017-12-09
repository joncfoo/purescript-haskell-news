(function() {
    var p = document.querySelector('audio');
    if (!p)
        return

    var rateEl = document.querySelector('#playbackRate');
    if (!rateEl)
        return
    rateEl.addEventListener('change', function(e) {
        p.playbackRate = Number(this.value)
        try {
            localStorage['playbackRate'] = this.value
        } catch (e) {}
    });

    try {
        var n = Number(localStorage['playbackRate'])
        if (Number.isFinite(n) && n > 0)
            p.playbackRate = n
        else
            p.playbackRate = 1

        rateEl.value = p.playbackRate
    } catch (e) {
        p.playbackRate = 1
    }

    var podcastsEl = document.querySelector('#podcasts');
    if (!podcastsEl)
        return
    podcastsEl.addEventListener('click', function(e) {
        if (!e.target)
            return
        if (!e.target.nodeName)
            return
        var link = e.target.getAttribute('data-enclosure')
        if (!link)
            return
        p.src = link
        p.play()
        e.preventDefault()
    });
})();
