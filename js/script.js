$(function () {
    moment.lang('ru');
    $('span[data-post-date]').each(function () {
        var $this = $(this);
        var date = moment($this.attr('data-post-date')).zone(moment().zone());
        var dateString = date.format("dddd, D MMMM YYYY, HH:mm");
        dateString = dateString.substring(0, 1).toLocaleUpperCase() + dateString.substring(1)
        $this.html(dateString);
        $this.attr('title', date.fromNow());
    });
});

hljs.initHighlightingOnLoad();
