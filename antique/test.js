$( function () {
    /* ========== Lazy Plugins ========== */
    
    Backbone.LazyCollectionView = Backbone.View.extend( {
        collectionView : {},
        
        _viewList : {},

        initialize : function( args ) {
            
            this.collection = args.collection;
            
            _.bindAll( this, "lazyAdd" );
            this.collection.on( "add", this.lazyAdd );
            _.bindAll( this, "lazyRemove" );
            this.collection.on( "remove", this.lazyRemove );
            _.bindAll( this, "lazyReset" );
            this.collection.on( "reset", this.lazyReset );
            _.bindAll( this, "expand" );
            
            this._viewList = new Array;
            
            this.expand( args );
            
        },
        
        lazyAdd : function( model ) {
            var view = new this.collectionView( { model : model } );
            this._viewList[model.cid] = view;
            this.lazyRender( view );
        },

        lazyRemove : function( model ) {
            this._viewList[model.cid].remove();
            delete this._viewList[model.cid];
        },

        lazyReset : function( e ) {
            for ( view in this._viewList ) {
                this._viewList[view].remove();
                delete this._viewList[view];
            }
        },

        lazyRender : function( view ) {},
        
        expand : function( args ) {}

    } );

    /* ========== Definition Stage ========== */
    
    var MyButton = Backbone.View.extend( {
        tagName: 'button',
        
        template: '<%= caption %>',
        
        events: {
            "click" : "onClick"
        },

        initialize: function( args ) {
            _.bindAll( this, "render" );
            _.bindAll( this, "expand" );
            this.caption = args.caption;
            this.expand( args );
        },

        render: function() {
            this.$el.html( _.template( this.template, { caption: this.caption } ) );
            return this;
        },
        
        onClick: function() {},
        
        expand: function( args ) {}
        
    } );
    
    
    var ButtonState = Backbone.Model.extend( {
        initialize : function ( args ) {
            this.caption = args.caption;
            this.msg = args.msg;
        }
    } );

    var SignalButton = MyButton.extend( {
        expand : function( args ) {
            this.model = args.model;
            this.msg = args.model.msg;
            this.caption = args.model.caption;
        },
        
        onClick: function() {
            this.model.trigger( "clicked", this.msg );
        }
    } );
    

    var StateSet = Backbone.Collection.extend( {
        model: ButtonState,

        initialize : function ( args ) {
            this.on( "clicked", this.decision );
        }, 
        
        decision : function( e ) {
            this.trigger( "killed", e );
            this.reset();
        }
    } );
    
    var ButtonPanel = Backbone.LazyCollectionView.extend( {
        
        collectionView : SignalButton,

        lazyRender : function( view ) {
            $('#main').append( view.render().el );
        }
    } );

    /* ========== Compile Stage ========== */
    
    var GenButton = MyButton.extend( {
        onClick: function() {
            
            var state_yes = new ButtonState( { msg : "yes", caption : "Yes" } );
            var state_no = new ButtonState( { msg : "no", caption : "No" } );
            var set = new StateSet;
            
            var panel = new ButtonPanel( { collection : set } );
            set.add( state_yes );
            set.add( state_no );
            
            this.undelegateEvents();
            set.on( "killed", function( e ) {
                // continuation starts here
                console.log( "the user clicked " + e );
                // continuation ends here
                this.delegateEvents();
            }, this );
        }
    } );

    var btn0 = new GenButton( { caption: "Generate" } );
    $('#main').append( btn0.render().el );
    
} );

				       