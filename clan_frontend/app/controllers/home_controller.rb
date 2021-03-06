class HomeController < ApplicationController
  def index
    redirect_to clan_index_path(:clan_id => session[:user_clan]) if
        view_context.user_signed_in?
  end

  def wot_logged_in
    if params['status'] == 'ok'
      Rails.logger.info(request.env)
      # TODO: Custom Erlang-based session store
      session[:user_nickname]      = params['nickname']
      session[:user_account]       = Integer(params['account_id'])
      session[:user_token] = token = params['access_token']
      session[:user_expires_at]    = Integer(params['expires_at']) + Time.now.to_i

      # Check user_token and fetch user info
      acc_id = Integer(params['account_id'])
      acc_info = UserModel.query_account_info(acc_id, token, 'en')
      session[:account_info] = acc_info
      session[:user_clan]    = acc_info['clan_id']

      # Fetching clan info
      clan_info = ClanModel.clan_info_2(session[:user_clan], session[:user_token], 'en')
      session[:clan_info] = clan_info

      m = clan_info['members'][session[:user_account].to_s]
      session[:is_clan_leader] = (not m.nil? and m['role'] == 'leader')

      redirect_to root_path
    else
      redirect_to root_path, :alert => t('app.login_error')
    end
  end

  def dev_logged_in
    redirect to root_path, alert: 'nope' unless Rails.env.development?

    tcid = Integer(params[:test_clan])
    session[:user_nickname]   = "testuser#{tcid}"
    session[:user_account]    = Integer(1000000000 + tcid)
    session[:user_token]      = "testtoken#{tcid}"
    session[:user_expires_at] = 86400 * 30 + Time.now.to_i
    session[:account_info]    = UserModel.test_acc_info(tcid)
    session[:user_clan]       = UserModel.test_acc_info(tcid)
    redirect_to root_path
  end

  def wot_log_out
    rpc = Rails.application.get_rpc
    rpc.call.ch_user_api.logged_out(session[:user_account])
    session.clear
  end

  before_filter :pre_check_new_site!, only: [:new_site, :new_site_create]
  def pre_check_new_site!
    if session[:clan_site_exists]
      redirect_to root_path, alert: t('app.site_exists')
    end
    unless session[:is_clan_leader]
      redirect_to root_path, alert: t('acl.no_rights_to_see_this')
    end
  end

  def new_site
    @clan_members = []
    session[:clan_info]['members'].each do |_, member|
      member_id = member['account_id']
      next if member_id == session[:user_account]
      @clan_members.append [member['account_name'], member_id]
    end
  end

  def new_site_create
    # update works as new too
    SiteModel.update(session[:user_clan],
                     :clan_id => session[:user_clan],
                     :clan_tag => session[:clan_info]['abbreviation'],
                     :clan_name => session[:clan_info]['name'])

    flash[:notice] = t('app.new_site.created')
    redirect_to root_path
  end

  def clan_search
    redirect_to request.referer,
                :alert => t('app.search.empty_request') if params['search'].empty?
    lang = session[:locale] || 'en'
    @vars = {}
    @vars[:clans] = ClanModel.search_clans(params['search'], lang)
    @vars[:clans].map! { |clan|
      formatted_title = "[#{clan['abbreviation']}] #{clan['name']}"
      if SiteModel.exists(clan['clan_id'])
        clan['site_exists'] = true
        clan['link_or_title'] =
            view_context.link_to formatted_title,
                                 clan_index_path(:clan_id => clan['clan_id'])
      else
        clan['site_exists'] = false
        clan['link_or_title'] = formatted_title
      end
      clan
    }

    @vars[:can_do_alliance] = AclModel.has_access(session[:user_clan],
                                                  'alliances',
                                                  session[:user_account])
  end

  def clan_index
    @vars = {}
    @vars[:clan_id]   = Integer(params[:clan_id])
    @vars[:clan_info] = ClanModel.clan_info(@vars[:clan_id])
  end

  def show_static
    redirect index_path unless params[:page_name].present?
    @vars = {}
    @vars[:page_name] = params[:page_name]
  end
end
