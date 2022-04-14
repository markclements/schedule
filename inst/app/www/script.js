$(document).ready(function() {
    var socket_timeout_interval
    var n = 0
    $(document).on('shiny:connected', function (event) {
        socket_timeout_interval = setInterval(function () {
            Shiny.onInputChange('count', n++)
        }, 15000)
    });
    $(document).on('shiny:disconnected', function (event) {
        clearInterval(socket_timeout_interval)
    });
    // From: https://stackoverflow.com/questions/36359553/display-leading-zeros-on-input-number-fields
    const hours = document.querySelector(".time_picker .hours input");
    const minutes = document.querySelector(".time_picker .minutes input");


    function addLeadingZero(value) {
        return value.length < 2 ? "0" + value : value;
    };

    hours.addEventListener("input", function () {
        hours.value = addLeadingZero(hours.value);
    });

    minutes.addEventListener("input", function () {
        minutes.value = addLeadingZero(minutes.value);
    });
// <input type="number" min="0" max="24" value="00" id="hours" class="hours">
// <input type="number" min="0" max="59" value="00" id="minutes" class="minutes"></input>
});

