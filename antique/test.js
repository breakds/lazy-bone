$( function () {
    /* ========== Lazy Plugins ========== */
    
    Backbone.LazyCollectionView = Backbone.View.extend( {
        collectionView : {},
        
        _viewList : {},

        initialize : function( args, expand ) {
            
            this.collection = args.collection;



            _.bindAll( this, "lazyAdd" );
            this.collection.on( "add", this.lazyAdd );
            _.bindAll( this, "lazyRemove" );
            this.collection.on( "remove", this.lazyRemove );
            _.bindAll( this, "lazyReset" );
            this.collection.on( "reset", this.lazyReset );
            _.bindAll( this, "lazyKill" );
            
            this._viewList = new Array;

            this.collection.each( this.lazyAdd );
            
            if ( undefined != expand ) {
                expand.call( this, args );
            }
            
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
        
        lazyKill : function( event ) {
            for ( view in this._viewList ) {
                this._viewList[view].remove();
                delete this._viewList[view];
            }
            this.trigger( "killed", event );
        }
    } );



    /* ========== Definition Stage ========== */
    
    var MyButton = Backbone.View.extend( {
        tagName: 'button',
        
        template: '<%= caption %>',
        
        events: {
            "click" : "onClick"
        },

        initialize: ( function( args, expand ) {
	    this.caption = args.caption;
            if ( undefined != expand ) {
                expand.call( this, args );
            }
        } ),

        render: function() {
            this.$el.html( _.template( this.template, { caption: this.caption } ) );
            return this;
        },
        
        onClick: function() {}
        
    } );
    
    
    var ButtonState = Backbone.Model.extend( {
        initialize : function ( args ) {
            this.caption = args.caption;
            this.msg = args.msg;
        }
    } );

    var SignalButton = MyButton.extend( {

        initialize : function( args, expand ) {
            this.constructor.__super__.initialize.call( this, args, function( args ) {
                this.model = args.model;
                this.msg = args.model.msg;
                this.caption = args.model.caption;
            } );
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
        },
        
        onClick: function() {
            this.model.trigger( "clicked", this.msg );
        }
    } );
    

    var StateSet = Backbone.Collection.extend( {
        model: ButtonState
    } );
    
    var ButtonPanel = Backbone.LazyCollectionView.extend( {
        
        collectionView : SignalButton,

        lazyRender : function( view ) {
            $('#main').append( view.render().el );
        },

        initialize : function ( args, expand ) {
            this.constructor.__super__.initialize.call( this, args, function( args ) {
                this.collection.on( "clicked", function( e ) {
                    this.collection.reset();
                    this.lazyKill( e );
                }, this );
		
            } );
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
        }
    } );

    /* ========== Compile Stage ========== */
    
    var GenButton = MyButton.extend( {
	initialize : function( args, expand ) {
	    this.constructor.__super__.initialize.call( this, args, function( args ) {
		$('#main').append( this.render().el );
            } );
	    if ( undefined != expand ) {
                expand.call( this, args );
            }
	},
        onClick: function() {

            var state_yes = new ButtonState( { msg : "yes", caption : "Yes" } );
            var state_no = new ButtonState( { msg : "no", caption : "No" } );
            
            var panel = new ButtonPanel( { collection : new StateSet( [state_yes, state_no] ) } );
            
            this.undelegateEvents();
            panel.on( "killed", function( e ) {
                // continuation starts here
                console.log( "the user clicked " + e );
                // continuation ends here
                this.delegateEvents();
            }, this );
        }
    } );

    var btn0 = new GenButton( { caption: "Generate" } );
    
    
} );

				       
