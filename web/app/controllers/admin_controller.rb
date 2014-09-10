class AdminController < ApplicationController
  before_action :pre_check_site_exists!
  # before_action :pre_check_acl!

  def pre_check_site_exists!
    unless session[:clan_site_exists]
      redirect_to root_path, :alert => t('app.site_is_not_created_notice')
    end
  end

  def index
    return unless require_clan_admin
    true
  end

  #----------------------------------------
  def newsfeed
    return unless require_acl('post_news')
    @news = NewsfeedItem::all_for_clan(session[:user_clan])
  end

  def newsfeed_add_form
    return unless require_acl('post_news')
    true
  end

  def newsfeed_delete
    return unless require_acl('post_news')

    NewsfeedItem::delete(session[:user_clan], params[:id])
    redirect_to manage_newsfeed_path, :notice => t('cp.newsfeed.deleted')
  end

  def newsfeed_add
    return unless require_acl('post_news')

    return redirect_to manage_newsfeed_add_form_path,
       :alert => t('cp.newsfeed.fill_at_least_short') if params[:short]==''
    fields = {:title => params[:title],
              :short => params[:short],
              :long  => params[:long] }
    NewsfeedItem::create(session[:user_clan], fields)
    redirect_to manage_newsfeed_path
  end
  #----------------------------------------

  def acl
    return unless require_clan_admin

    ## member[0] is user name, member[1] is user id
    @clan_members = Clan::get_members_helper session[:clan_info]
    @clan_acl = {}
    @clan_members.each do |member|
      @clan_acl[member[1]] = AccessRights::read_for_user(session[:user_clan], member[1]) || []
    end
  end

end
