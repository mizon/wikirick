<apply template="base">
  <bind tag="wiki:js-libs">
    <script src="/js/jquery.js"/>
    <script src="/js/underscore.js"/>
    <script src="/js/backbone.js"/>
    <script src="/js/wikirick/index.js"/>
  </bind>
  <nav id="navigation"/>
  <div id="container" class="container_16">
    <header id="page-title" class="grid_16"><h1><wiki:title/></h1></header>
    <div class="aside grid_3">
      <aside>
        <header><h2>Recent Updates</h2></header>
        <ul>
          <wiki:recent-updates/>
        </ul>
      </aside>
    </div>
    <div id="main" class="grid_13">
      <wiki:content/>
    </div>
    <hr class="clear"/>
    <footer class="grid_16">
      <p>This site is powered by Wikirick</p>
    </footer>
  </div>
</apply>
